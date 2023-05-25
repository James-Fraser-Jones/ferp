module Settings where

import Graphics.Gloss.Data.Color (Color, black)

-----------------------------------------------------------------------------
--Gloss startup settings

windowName :: String
windowName = "Ferp"

windowSize :: (Int, Int)
windowSize = (640, 480)

windowOffset :: (Int, Int)
windowOffset = (200, 200)

background :: Color
background = black

fps :: Int
fps = 30

-----------------------------------------------------------------------------