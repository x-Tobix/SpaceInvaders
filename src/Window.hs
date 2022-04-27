module Window
  ( window
  , background
  , fps
  ) where

import Graphics.Gloss ( Color, Display(InWindow), black )

window :: Display
window = InWindow "Space Invaders" (960, 640) (300, 100)

background :: Color
background = black

fps :: Int
fps = 60
