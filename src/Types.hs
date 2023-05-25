module Types (Keyboard, Mouse(..), Input(..), Signal) where

import Control.Monad.Reader (Reader)
import Graphics.Gloss.Interface.Pure.Game (KeyState)

-----------------------------------------------------------------------------
--Types (just for reading, these shouldn't need to change at all)

import Keyboard (Keyboard) --Keyboard type is purposefully opaque, no constructors exported from Keyboard.hs

data Mouse = Mouse {position :: (Float, Float), left :: KeyState, right :: KeyState, middle :: KeyState, wheel :: Int}
data Input = Input {mouse :: Mouse, keyboard :: Keyboard}

type Signal a = Reader Input a

-----------------------------------------------------------------------------