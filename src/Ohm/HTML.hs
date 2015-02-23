{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
module Ohm.HTML
  ( -- * bootstrap operators
    bootstrapEl
  , container, row, col3, col6, col9

    -- * Utilities
  , mkButton


    -- * Re-exported modules
  , module Ohm.Internal.HTML
  ) where
import Prelude hiding (div, head, map, mapM, sequence, span)
import Ohm.Internal.HTML
import Control.Applicative
import Control.Lens
--import Data.Profunctor

bootstrapEl :: String -> [HTML] -> HTML
bootstrapEl cls = with div (classes .= [cls])

bsCol :: Int -> [HTML] -> HTML
bsCol n = bootstrapEl $ "col-sm-" ++ (show n)

container, row, col3, col6, col9 :: [HTML] -> HTML
container = bootstrapEl "container"
row = bootstrapEl "row"
col3 = bsCol 3
col6 = bsCol 6
col9 = bsCol 9

mkButton :: IO () -> HTML -> [String] -> HTML
mkButton io btnTxt classList =
  with button
    (do classes .= ["button", "btn"] ++ classList
        onClick io)
    [btnTxt]
