module Main where

import SpaceInvaders ( startLevel, update, AppState(Menu) )
import GlossRenderer
    ( update,
      glossEventToGameAction,
      keyHandler,
      renderGame,
      background,
      fps,
      window,
      ObjectBitmaps(..) )
import Graphics.Gloss (loadBMP, play, Display (InWindow), display, makeColor, Picture(Text))
import Graphics.Gloss.Data.Color

loadBitmaps :: IO ObjectBitmaps
loadBitmaps = do
  bkgImg <- loadBMP "./objects/background.bmp"
  shipImg <- loadBMP "./objects/spaceship.bmp"
  mstImg <- loadBMP "./objects/invader.bmp"
  return $ ObjectBitmaps bkgImg shipImg mstImg

main :: IO ()
main = 
  do
  imgs <- loadBitmaps
  play
    window
    background
    fps
    (startLevel Menu 1 0 (0, -270) [] [])
    (renderGame imgs)
    (keyHandler . glossEventToGameAction)
    update
