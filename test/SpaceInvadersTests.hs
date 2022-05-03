module SpaceInvadersTests where
    import Test.QuickCheck ( quickCheck, Arbitrary, elements, chooseInt, choose, Gen, oneof, forAll, listOf, listOf1, NonEmptyList (getNonEmpty) )
    import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))
    import SpaceInvaders (Game (Game), GameAction(..), AppState (..), Spaceship (Spaceship), Invader (Invader), Missile (Missile), InvaderDirection (LeftDir, RightDir), startLevel, actionHandler, removeItem, missileRadius, isEmpty, Rectangle (Rectangle), Circle (Circle), recCircIntersect, removeIf, moveInvadersMissiles, moveInvaders, moveMissiles, moveSpaceshipRight, moveSpaceshipLeft)
    import System.Random ( mkStdGen, StdGen )

    spaceInvadersTests :: IO()
    spaceInvadersTests = quickCheck actionHandlerTest
        >> quickCheck isEmptyReturnsTrueIfEmpty
        >> quickCheck isEmptyReturnsFalseWhenNotEmpty
        >> quickCheck recCircIntersectTest
        >> quickCheck removeItemTest
        >> quickCheck moveInvadersMissilesRemovesMissileWhenIsOutOfBoundaries
        >> quickCheck moveInvadersMissilesRemovesMissileWhenHitSpaceship
        >> quickCheck moveMissilesRemovesMissileWhenIsOutOfBoundaries
        >> quickCheck moveMissilesRemovesMissileWhenHitInvader
        >> quickCheck moveInvadersDeletesInvaderWhenMissileHits
        >> quickCheck spaceshipStopsMovingWhenHitRightWall
        >> quickCheck spaceshipStopsMovingWhenHitLeftWall

    actionHandlerTest :: GameAction -> Game -> Bool
    actionHandlerTest Restart game@(Game Lost _ _ _ _ _ _ _ _ _ _) = actionHandler Restart game == startLevel Menu 1 0 (0, -270) [] []
    actionHandlerTest Restart game = actionHandler Restart game == game
    actionHandlerTest ga game@(Game Lost _ _ _ _ _ _ _ _ _ _) = actionHandler ga game == game
    actionHandlerTest Start game@(Game Menu spaceship invaders movs mis emis g c s p l) = actionHandler Start game == Game Running spaceship invaders movs mis emis g c s p l
    actionHandlerTest Start game@(Game Running _ _ _ _ _ _ _ _ _ _) = actionHandler Start game == game
    actionHandlerTest ga game@(Game Menu _ _ _ _ _ _ _ _ _ _) = actionHandler ga game == game
    actionHandlerTest Quit game = actionHandler Quit game == startLevel Menu 1 0 (0, -270) [] []
    actionHandlerTest PushLeft game@(Game Running spaceship invaders movs mis emis g c s p l) = actionHandler PushLeft game == Game Running spaceship invaders (movs ++ [PushLeft]) mis emis g c s p l
    actionHandlerTest PushRight game@(Game Running spaceship invaders movs mis emis g c s p l) = actionHandler PushRight game == Game Running spaceship invaders (movs ++ [PushRight]) mis emis g c s p l
    actionHandlerTest UnpushLeft game@(Game Running spaceship invaders movs mis emis g c s p l) = actionHandler UnpushLeft game == Game Running spaceship invaders (removeItem PushLeft movs) mis emis g c s p l
    actionHandlerTest UnpushRight game@(Game Running spaceship invaders movs mis emis g c s p l) = actionHandler UnpushRight game == Game Running spaceship invaders (removeItem PushRight movs) mis emis g c s p l
    actionHandlerTest Shoot game@(Game Running (Spaceship (x,y)) invaders movs mis emis g c s p l) = actionHandler Shoot game == Game Running (Spaceship (x,y)) invaders movs (mis ++ [Missile (x,y + 40) 10 missileRadius]) emis g c s p l
    actionHandlerTest Pause game@(Game Running spaceship invaders movs mis emis g c s p l) = actionHandler Pause game == Game Running spaceship invaders movs mis emis g c s (not p) l

    spaceshipStopsMovingWhenHitLeftWall :: Float -> Bool
    spaceshipStopsMovingWhenHitLeftWall spX
        | (spX - 5) < -455 = Spaceship (-455,0) == moveSpaceshipLeft (Game Running (Spaceship (spX, 0)) [] [PushLeft] [] [] (mkStdGen 0) 1 0 False 1)
        | otherwise = Spaceship (spX - 5,0) == moveSpaceshipLeft (Game Running (Spaceship (spX,0)) [] [PushLeft] [] [] (mkStdGen 0) 1 0 False 1)

    spaceshipStopsMovingWhenHitRightWall :: Float -> Bool 
    spaceshipStopsMovingWhenHitRightWall spX
        | (spX + 5) > 455 = Game Running (Spaceship (455,0)) [] [PushRight] [] [] (mkStdGen 0) 1 0 False 1 == moveSpaceshipRight (Game Running (Spaceship (spX,0)) [] [PushRight] [] [] (mkStdGen 0) 1 0 False 1)
        | otherwise = Game Running (Spaceship (spX + 5,0)) [] [PushRight] [] [] (mkStdGen 0) 1 0 False 1 == moveSpaceshipRight (Game Running (Spaceship (spX,0)) [] [PushRight] [] [] (mkStdGen 0) 1 0 False 1)

    moveInvadersDeletesInvaderWhenMissileHits :: Float -> Float -> Float -> Float -> Bool
    moveInvadersDeletesInvaderWhenMissileHits invX invY misX misY
        | recCircIntersect (Rectangle (invX, invY) 68 82) (Circle (misX, misY) 5) = null (fst (moveInvaders [Invader (invX, invY) invX LeftDir] [Missile (misX, misY) 10 5]))
        | otherwise = fst (moveInvaders [Invader (invX, invY) invX LeftDir] [Missile (misX, misY) 10 5]) /= []

    moveMissilesRemovesMissileWhenHitInvader :: Float -> Float -> Float -> Float -> Bool
    moveMissilesRemovesMissileWhenHitInvader invX invY misX misY
        | misY < 320  && recCircIntersect (Rectangle (invX, invY) 68 82) (Circle (misX, misY) 5) = null (moveMissiles [Missile (misX, misY) 10 5] [Invader (invX, invY) invX LeftDir])
        | misY < 320 && not (recCircIntersect (Rectangle (invX, invY) 68 82) (Circle (misX, misY) 5)) = moveMissiles [Missile (misX, misY) 10 5] [Invader (invX, invY) invX LeftDir] /= []
        | otherwise = null (moveMissiles [Missile (misX, misY) 10 5] [Invader (invX, invY) invX LeftDir])

    moveMissilesRemovesMissileWhenIsOutOfBoundaries :: Float -> Bool
    moveMissilesRemovesMissileWhenIsOutOfBoundaries misY
        | misY > 320 = null (moveMissiles [Missile (0, misY) 10 5] [])
        | otherwise = moveMissiles [Missile (0, misY) 10 5] [] /= []

    moveInvadersMissilesRemovesMissileWhenHitSpaceship :: Float -> Float -> Float -> Float -> Bool
    moveInvadersMissilesRemovesMissileWhenHitSpaceship emisX emisY spX spY
        | emisY > -320  && recCircIntersect (Rectangle (spX, spY) 36 58) (Circle (emisX, emisY) 5) = null (moveInvadersMissiles (Game Running (Spaceship (spX, spY)) [] [] [] [Missile (emisX, emisY) 10 5] (mkStdGen 0) 0 0 False 0))
        | emisY > -320 && not (recCircIntersect (Rectangle (spX, spY) 36 58) (Circle (emisX, emisY) 5)) = moveInvadersMissiles (Game Running (Spaceship (spX, spY)) [] [] [] [Missile (emisX, emisY) 10 5] (mkStdGen 0) 0 0 False 0) /= []
        | otherwise = null (moveInvadersMissiles (Game Running (Spaceship (spX, spY)) [] [] [] [Missile (emisX, emisY) 10 5] (mkStdGen 0) 0 0 False 0))

    moveInvadersMissilesRemovesMissileWhenIsOutOfBoundaries :: Float -> Bool
    moveInvadersMissilesRemovesMissileWhenIsOutOfBoundaries emisY
        | emisY < -320 = null (moveInvadersMissiles (Game Running (Spaceship (0, 0)) [] [] [] [Missile (0, emisY) 10 5] (mkStdGen 0) 0 0 False 0))
        | otherwise = moveInvadersMissiles (Game Running (Spaceship (10000, 10000)) [] [] [] [Missile (0, emisY) 10 5] (mkStdGen 0) 0 0 False 0) /= []

    removeItemTest :: Int -> [Int] -> Bool
    removeItemTest el list = el `notElem` removeItem el list

    recCircIntersectTest :: Rectangle -> Circle -> Bool
    recCircIntersectTest re@(Rectangle (x, y) w h) c@(Circle o r)
        | abs (fst o - x) > (w/2 + r) || abs (snd o - y) > (h/2 + r) = not (recCircIntersect re c)
        | abs (fst o - x) <= w/2 || abs (snd o - y) <= h/2 = recCircIntersect re c
        | otherwise = (cornerDistance <= r ** 2) == recCircIntersect re c
        where
            cornerDistance = (abs (fst o - x) - w/2) ** 2 + (abs (snd o - y) - h/2) ** 2

    isEmptyReturnsTrueIfEmpty :: Bool
    isEmptyReturnsTrueIfEmpty = isEmpty []

    isEmptyReturnsFalseWhenNotEmpty :: NonEmptyList Int -> Bool
    isEmptyReturnsFalseWhenNotEmpty list = not (isEmpty $ getNonEmpty list)

----------ARBITRARY----------
    instance Arbitrary GameAction where
        arbitrary = elements [Quit, PushLeft, PushRight, UnpushLeft, UnpushRight, Shoot, Pause, Start, Restart]

    instance Arbitrary Game where
        arbitrary = do
            appstate <- elements [Menu, Running, Lost]
            spaceshipX <- chooseFloat (-455, 455)
            invaderStartX <- chooseFloat (-400, 400)
            invaderX <- chooseFloat (invaderStartX - 10, invaderStartX + 10)
            invaderY <- chooseFloat (-320, 320)
            invaderDirection <- elements [LeftDir, RightDir]
            isPushLeft <- chooseBool
            isPushRight <- chooseBool
            misX <- chooseFloat (-455, 455)
            misY <- chooseFloat (-230, 330)
            invMisX <- chooseFloat (-320, 320)
            invMisY <- chooseFloat (-455, 455)
            randomGen <- chooseInt(1, 10000)
            invAttackChance <- chooseFloat(0, 1000)
            score <- chooseInt(0, 1000)
            pause <- chooseBool
            level <- chooseInt(0, 1000)
            return (Game appstate (Spaceship(spaceshipX, -270)) [Invader (invaderX, invaderY) invaderStartX invaderDirection] (addIf (addIf [] PushLeft isPushLeft) PushRight isPushRight) [Missile (misX, misY) 10 5] [Missile (invMisX, invMisY) 10 5] (mkStdGen randomGen) invAttackChance score pause level)

    instance Arbitrary Rectangle where
        arbitrary = do
            posX <- chooseFloat (-500, 500)
            posY <- chooseFloat (-500, 500)
            width <- chooseFloat (0, 500)
            height <- chooseFloat (0, 500)
            return $ Rectangle (posX, posY) width height

    instance Arbitrary Circle where
        arbitrary = do
            posX <- chooseFloat (-500, -500)
            posY <- chooseFloat (-500, 500)
            radius <- chooseFloat (0, 500)
            return $ Circle (posX, posY) radius

----------HELPERS----------
    chooseBool :: Gen Bool
    chooseBool = oneof [return True, return False]

    chooseFloat :: (Float, Float) -> Gen Float
    chooseFloat = choose

    addIf :: [a] -> a -> Bool -> [a]
    addIf list elt b = if b then list++[elt] else list
