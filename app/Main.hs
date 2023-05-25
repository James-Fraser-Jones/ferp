module Main (Main.main) where

import Control.Monad.Reader
import Graphics.Gloss.Interface.Pure.Game

import Types
import Settings
import qualified Keyboard as Kb (empty, insert)

import Lib as Lib (main)

data World = World {input :: Input, output :: Picture}

main :: IO ()
main = play displaySettings background fps initWorld 
  output
  (\e w -> w {input = updateInput e $ input w, output = runReader Lib.main $ input w}) 
  (const id)

updateInput :: Event -> Input -> Input
updateInput e i = case e of
  EventKey k s _ _ -> case k of
    MouseButton m -> case m of
      LeftButton -> i {mouse = (mouse i) {left = s}}
      RightButton -> i {mouse = (mouse i) {right = s}}
      MiddleButton -> i {mouse = (mouse i) {middle = s}}
      WheelUp -> i {mouse = (mouse i) {wheel = (wheel $ mouse i) - 1}}
      WheelDown -> i {mouse = (mouse i) {wheel = (wheel $ mouse i) + 1}}
      _ -> i
    _ -> i {keyboard = Kb.insert k s $ keyboard i}
  EventMotion p -> i {mouse = (mouse i) {position = p}}
  _ -> i

initWorld :: World
initWorld = World (Input (Mouse (0,0) Up Up Up 0) Kb.empty) blank

displaySettings :: Display
displaySettings = InWindow windowName windowSize windowOffset