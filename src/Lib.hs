module Lib (main) where

-----------------------------------------------------------------------------
--standard imports for signal modules

import Control.Monad.Reader               --Signals are Readers
import Graphics.Gloss.Interface.Pure.Game --Gloss Outputs

import Types                              --Inputs and Signals
import Settings                           --Gloss startup settings
import qualified Keyboard as Kb (lookup)  --Utility to query Keyboard Input

-----------------------------------------------------------------------------
--Lib.hs always exports "main" as entry point to program

main :: Signal Input -> Signal Picture
main = example

-----------------------------------------------------------------------------

--during each sample frame, the input signal is given as the constant signal for the current sampled input
example :: Signal Input -> Signal Picture
example input = do
  i <- input
  let c = makeColor r g 0 1
      r = (x + w/2)/w
      g = (y + h/2)/h
      (x, y) = position $ mouse i
      (w, h) = (fromIntegral $ fst windowSize, fromIntegral $ snd windowSize)
  pure $ Color c (thickCircle 0 300)