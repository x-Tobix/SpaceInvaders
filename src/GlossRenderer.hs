module GlossRenderer
  ( ObjectBitmaps(..)
  , renderGame
  , glossEventToGameAction
  , renderScores
  , keyHandler
  , update
  , module Window
  ) where

  import SpaceInvaders
    ( Game(spaceship, invaders, missiles, invadersMissiles, score, appstate),
      GameAction(..),
      Invaders,
      Invader(..),
      Spaceship(..),
      Missile(..),
      Missiles,
      InvadersMissiles,
      update,
      actionHandler,
      AppState(..) )

  import Window
  import Graphics.Gloss ( circleSolid, pictures, translate, Picture (Text), Color, color, black, red, scale, white, rectangleWire )
  import Graphics.Gloss.Interface.IO.Game
    ( white,
      scale,
      red,
      black,
      color,
      translate,
      pictures,
      circleSolid,
      SpecialKey(KeySpace),
      KeyState(Down, Up),
      Key(Char, MouseButton, SpecialKey),
      Event(EventKey),
      Color,
      Picture(Text),
      rectangleWire,
      MouseButton(LeftButton) )  

  data ObjectBitmaps = ObjectBitmaps
    { backgroundBmp :: Picture
    , spaceshipBmp :: Picture
    , invaderBmp :: Picture
    }

  renderGame :: ObjectBitmaps -> Game -> Picture
  renderGame objBmp game
    | appstate game == Menu =
    pictures
    [ renderStartRec
    , renderStartText
    , renderExitText
    ]
    | appstate game == Lost =
    pictures
    [ renderLostMessage
    , renderEndScore
    , renderRestartRec
    , renderRestartText
    , renderExitText
    ]
    | otherwise =
    pictures
    [ renderedBackground
    , renderedSpaceship
    , renderedInvaders
    , renderedMissiles
    , renderedInvadersMissiles
    , renderedScore
    ]
    where
        renderedBackground = renderBackground (backgroundBmp objBmp)
        renderedSpaceship
          = renderSpaceship (spaceship game) (spaceshipBmp objBmp)
        renderedInvaders
          = renderInvaders (invaders game) (invaderBmp objBmp)
        renderedMissiles = renderMissiles black (missiles game)
        renderedInvadersMissiles
          = renderMissiles red (invadersMissiles game)
        renderedScore = renderScores (450, 290) (0.2, 0.2) (score game)
        renderStartRec
          = color white (translate 0 200 (rectangleWire 800 50))
        renderStartText
          = scale
              0.4 0.4 (translate (- 150) 450 (color white (Text "Start")))
        renderExitText
          = scale
              0.4 0.4
              (translate
                 (- 600) (- 550) (color white (Text "Press 'Esc' to exit")))
        renderLostMessage
          = scale
              0.4 0.4 (translate (-300) 450 (color white (Text "You Lost!")))
        renderEndScore = renderScores (moveRenderScore $ score game, 0) (0.4, 0.4) (score game)
        renderRestartRec
          = color white (translate 0 (-100) (rectangleWire 800 50))
        renderRestartText
          = scale
              0.4 0.4
              (translate
                 (- 400) (-300) (color white (Text "Go to menu")))
        moveRenderScore score
          | score < 10 = 0
          | score >= 10 && score < 100 = -20
          | score >= 100 && score < 1000 = -40
          | score >= 1000 && score < 10000 = -60
          | otherwise = -80  


  renderBackground :: Picture -> Picture
  renderBackground bkgBmp = bkgBmp

  renderSpaceship :: Spaceship -> Picture -> Picture
  renderSpaceship (Spaceship (x, y)) = translate x y

  renderInvader :: Picture -> Invader -> Picture
  renderInvader bmp (Invader (x, y) _ _) = translate x y bmp

  renderInvaders :: Invaders -> Picture -> Picture
  renderInvaders invs bmp = pictures $ fmap renderInvaderWithImg invs
    where renderInvaderWithImg = renderInvader bmp

  renderScores :: (Float, Float) -> (Float, Float) -> Int -> Picture
  renderScores (x,y) (s1, s2) s = color white $ translate x y $ scale s1 s2 $ Text (show s)

  renderMissile :: Color -> Missile -> Picture
  renderMissile c (Missile (x,y) v r) = color c $ translate x y $ circleSolid r

  renderMissiles :: Color -> Missiles -> Picture
  renderMissiles c mis = pictures $ fmap (renderMissile c) mis

  glossEventToGameAction :: Event -> Maybe GameAction
  glossEventToGameAction (EventKey (MouseButton LeftButton) Up _ (x, y))
    | x <= 400 && x >= -400 && y <= 225 && y >= 175 =
    Just Start
    | x <= 400 && x >= -400 && y <= -75 && y >= -125 =
    Just Restart
    | otherwise = Nothing
  glossEventToGameAction (EventKey (Char 'q') Down _ _) = Just Quit
  glossEventToGameAction (EventKey (Char 'a') Down _ _) = Just PushLeft
  glossEventToGameAction (EventKey (Char 'a') Up _ _) = Just UnpushLeft
  glossEventToGameAction (EventKey (Char 'd') Down _ _) = Just PushRight
  glossEventToGameAction (EventKey (Char 'd') Up _ _) = Just UnpushRight
  glossEventToGameAction (EventKey (SpecialKey KeySpace) Down _ _) = Just Shoot
  glossEventToGameAction (EventKey (Char 'p') Down _ _) = Just Pause
  glossEventToGameAction _ = Nothing

  keyHandler :: Maybe GameAction -> Game -> Game
  keyHandler Nothing game = game
  keyHandler (Just k) game = actionHandler k game
