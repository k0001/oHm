{-# LANGUAGE RankNTypes #-}

module Ohm
  ( run
  ) where

import           Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Control.Monad.Trans.State as State
import qualified Lei as Lei

import qualified Ohm.HTML as HTML

--------------------------------------------------------------------------------


run
  :: (MonadIO m, Eq v, Eq s,
      --remove
      Show v, Show s)
  => (forall x y z . m x -> (x -> m y) -> (x -> m z) -> m z)
  -- ^ 'Control.Exception.bracket'-like function.
  --    Hint: use 'Control.Monad.Catch.bracket' from "Control.Monad.Catch".
  -> (IO (Lei.Debug v r s) -> IO a)
  -- ^ Loop monitoring events.
  -> s
  -- ^ Initial model state.
  -> Lei.Controller r s r s m
  -- ^ /Controller/ issuing controller requests and model operations.
  -> Lei.View v r s m HTML.HTML
  -- ^ /View/ issuing controller requests and rendering the model.
  -> m ()
run bracket mon s0 cr avw = Lei.run bracket mon s0 cr $ Lei.mkView $ do
     tlc <- liftIO HTML.newTopLevelContainer
     (av0, avr) <- Lei.nestView (fmap id .) id avw
     let vs _ = error "TODO: destroy ohm top level container"
         kvr = \_ s -> HTML.renderTo tlc `fmap` Lei.render avr s
     return (av0, vs, kvr)
