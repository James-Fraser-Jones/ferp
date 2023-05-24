module Main (main) where

--------------------------------------------------------------------------------------------------------------------------------------
--imports

import Graphics.Gloss.Interface.Pure.Game

import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

--------------------------------------------------------------------------------------------------------------------------------------
--types

data World = World {input :: Input, output :: Output}

newtype Output = Output {unOutput :: Picture}

data Input = Input {mouse :: Mouse, keyboard :: Keyboard} deriving (Show)
newtype Keyboard = Keyboard {unKeyboard :: Map Key KeyState} deriving (Show)
data Mouse = Mouse {position :: (Float, Float), left :: KeyState, right :: KeyState, middle :: KeyState, wheel :: Int} deriving (Show)

newtype Signal a = Signal {unSignal :: Input -> a}

--------------------------------------------------------------------------------------------------------------------------------------
--settings

windowName :: String
windowName = "Ferp"

windowSize :: (Int, Int)
windowSize = (640, 480)

background :: Color
background = black

fps :: Int
fps = 30

--------------------------------------------------------------------------------------------------------------------------------------
--main

displaySettings :: Display
displaySettings = InWindow windowName windowSize (200, 200)

initWorld :: World
initWorld = World (Input (Mouse (0,0) Up Up Up 0) (Keyboard Map.empty)) (Output blank)

main :: IO ()
main = play displaySettings background fps initWorld 
  (\w -> unOutput $ output w) 
  (\e w -> w {input = updateInput e $ input w, output = unSignal outputSignal $ input w}) 
  (\_ w -> w)

updateInput :: Event -> Input -> Input
updateInput e i = traceShow i $ case e of 
  EventKey k s _ _ -> case k of
    MouseButton m -> case m of
      LeftButton -> i {mouse = (mouse i) {left = s}}
      RightButton -> i {mouse = (mouse i) {right = s}}
      MiddleButton -> i {mouse = (mouse i) {middle = s}}
      WheelUp -> i {mouse = (mouse i) {wheel = (wheel $ mouse i) - 1}}
      WheelDown -> i {mouse = (mouse i) {wheel = (wheel $ mouse i) + 1}}
      _ -> i
    _ -> i {keyboard = (keyboard i) {unKeyboard = Map.insert k s (unKeyboard $ keyboard i)}}
  EventMotion p -> i {mouse = (mouse i) {position = p}}
  _ -> i

--------------------------------------------------------------------------------------------------------------------------------------
--signals

outputSignal :: Signal Output
outputSignal = Signal $ \i -> 
  Output $ Color red $ Circle 400