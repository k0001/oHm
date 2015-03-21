{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Ohm
  ( run
  ) where

import           Control.Applicative (Applicative(pure, (<*>)), (<$>), (<*))
import           Control.Lens (preview, set, (.=), _1, _2)
import           Control.Monad (liftM, void, ap)
import           Control.Monad.Morph (hoist)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Control.Monad.State as State
import qualified Data.Aeson as Ae
import           Data.Monoid (Monoid, mempty, mconcat)
import qualified Lei as Lei
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.DOMWindow as DOMWindow
import qualified GHCJS.DOM.EventM as DOMEvent
import qualified GHCJS.DOM.History as DOMHistory
import qualified GHCJS.DOM.Location as DOMLocation
import qualified GHCJS.DOM.Node as DOMNode
import qualified GHCJS.DOM.PopStateEvent as DOMPopStateEvent
import           GHCJS.Foreign (FromJSString, ToJSString(toJSString))
import           GHCJS.Marshal (fromJSRef, ToJSRef(toJSRef))
import           GHCJS.Types (JSString)
import qualified Ohm.HTML as HTML


--------------------------------------------------------------------------------

newtype BrowserT route state m a
  = BrowserT { unBrowserT :: DOMWindow.DOMWindow -> (route -> JSString) -> m a }
  deriving (Functor)

runBrowserT
  :: BrowserT route state m a
  -> DOMWindow.DOMWindow
  -> (route -> JSString)
  -> m a
runBrowserT = unBrowserT

instance Monad m => Monad (BrowserT route state m) where
  return = \a -> BrowserT (\_ _ -> return a)
  {-# INLINE return #-}
  m >>= k = BrowserT (\win toURL -> do
     a <- unBrowserT m win toURL
     unBrowserT (k a) win toURL)
  {-# INLINE (>>=) #-}

instance (Functor m, Monad m) => Applicative (BrowserT route state m) where
  pure = return
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance MonadIO m => MonadIO (BrowserT route state m) where
  liftIO = \m -> BrowserT (\_ _ -> liftIO m)
  {-# INLINE liftIO #-}

instance MonadTrans (BrowserT route state) where
  lift = \m -> BrowserT (\_ _ -> m)
  {-# INLINE lift #-}

class MonadIO m => MonadBrowser route state m | m -> route, m -> state where
  browser :: (DOMWindow.DOMWindow -> (route -> JSString) -> IO a) -> m a

instance MonadIO m => MonadBrowser route state (BrowserT route state m) where
  browser = BrowserT . fmap (fmap liftIO)
  {-# INLINE browser #-}

instance MonadBrowser route state m => MonadBrowser route state (Lei.C r0 s0 r s m) where
  browser = lift . browser
  {-# INLINE browser #-}

historyReplaceState
  :: (MonadBrowser route state m, Ae.ToJSON state)
  => state
  -> Maybe route
  -> m ()
historyReplaceState = \state route -> browser $ \win toURL -> do
    Just his <- DOMWindow.getHistory win
    sRef <- toJSRef (Ae.toJSON state)
    DOMHistory.replaceState his sRef ("" :: String) (maybe "" toURL route)

historyPushState
  :: (MonadBrowser route state m, Ae.ToJSON state)
  => state
  -> Maybe route
  -> m ()
historyPushState = \state route -> browser $ \win toURL -> do
    Just his <- DOMWindow.getHistory win
    sRef <- toJSRef (Ae.toJSON state)
    DOMHistory.pushState his sRef ("" :: String) (maybe "" toURL route)

--------------------------------------------------------------------------------

run
  :: forall m v s route routeURL r r0 s0
   . (MonadIO m, Eq v, Eq s, Eq route, ToJSString routeURL, Ae.ToJSON s, Ae.FromJSON s)
  => (forall n x y z. n x -> (x -> n y) -> (x -> n z) -> n z)
  -- ^ 'Control.Exception.bracket'-like function.
  --    Hint: use 'Control.Monad.Catch.bracket' from "Control.Monad.Catch".
  -> (IO (Lei.Debug v (Either s r) s) -> IO ())
  -- ^ Loop monitoring events.
  -> s
  -- ^ Initial model state.
  -> Lei.Controller r s r s (BrowserT route s m)
  -- ^ /Controller/ issuing controller requests and model operations.
  -> Lei.View v r s m HTML.HTML
  -- ^ /View/ issuing controller requests and rendering the model.
  -> (route -> routeURL)
  -- ^ Render routes as relative URLS (starting with "/").
  -> DOMWindow.DOMWindow
  -- ^ Browser window to take control of.
  -> m ()
run bracket mon s0 cr avw toRelURL win = do
    runBrowserT
       (Lei.run bracket mon s0 crRoot (hoist lift viewRoot))
       win (toJSString . toRelURL)
  where
    -- Our top level controller supports directly setting the state @s@, this
    -- will be used to change @s@ after DOM PopState events.
    crRoot :: Lei.Controller (Either s r) s (Either s r) s (BrowserT route s m)
    crRoot = Lei.mkController $ \r -> case r of
      Left s -> id .= s
      Right r' -> Lei.nestController0 cr (preview id, set id) Right r'

    viewRoot :: Lei.View v (Either s r) s m (IO ())
    viewRoot = Lei.mkView $ \req _ -> do
      (av0, avr) <- Lei.nestView id Right avw
      removePS <- liftIO $ DOMEvent.on win DOMWindow.popState $ do
         rawStateRef <- DOMPopStateEvent.getState =<< DOMEvent.event
         ms <- liftIO $ fromJSRef rawStateRef
         case Ae.fromJSON <$> ms of
            Just (Ae.Success s) -> liftIO (req (Left s))
            _ -> error "popState: TODO deal with UNEXPECTED event state"
      Just doc <- DOMWindow.getDocument win
      (el, vp) <- liftIO $ HTML.newTopLevelContainer doc
      return ( av0
             , const (DOMNode.removeChild doc (Just el) >> removePS)
             , fmap (HTML.renderTo vp) . Lei.render avr
             )

--------------------------------------------------------------------------------
-- Internal tools

-- | Get relative URL, including path, query-string and hash.
-- getRelativeURL
--   :: (MonadIO m, FromJSString a, Monoid a)
--   => DOMLocation.Location -> m a
-- getRelativeURL loc = liftM mconcat $ mapM (\f -> maybe mempty id `liftM` f loc)
--     [DOMLocation.getPathname, DOMLocation.getSearch, DOMLocation.getHash]
