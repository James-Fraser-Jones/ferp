module Keyboard (Keyboard, empty, insert, Keyboard.lookup) where

import Data.Map (Map)
import qualified Data.Map as Map
import Graphics.Gloss.Interface.Pure.Game (Key, KeyState(..))

newtype Keyboard = Keyboard {keys :: Map Key KeyState}

empty :: Keyboard
empty = Keyboard $ Map.empty

insert :: Key -> KeyState -> Keyboard -> Keyboard
insert k s kb = Keyboard $ Map.insert k s $ keys kb

lookup :: Key -> Keyboard -> KeyState
lookup k kb = Map.findWithDefault Up k $ keys kb