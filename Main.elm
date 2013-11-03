module Maze where

import Mouse
import Touch
import Window


-- model configuration

(gameWidth,gameHeight) = (200,200)
(halfWidth,halfHeight) = (toFloat gameWidth / 2, toFloat gameHeight / 2)
playerRadius = 5


type RawLevelKnot = ((Float,Float),Float)
rawLevelKnot x y r = ((x,y),r)

type RawLevel = [((Float,Float),Float)]

levelsRaw : [RawLevel]
levelsRaw =
  [
    [
      rawLevelKnot -70 -70  4
    , rawLevelKnot  30  70  3.5
    , rawLevelKnot   0 -70  2
    ]
  , [
      rawLevelKnot   0 -70  4
    , rawLevelKnot   0   0  3.6
    , rawLevelKnot  50   0  3.0
    , rawLevelKnot  50  40  2.4
    , rawLevelKnot   0  40  2
    , rawLevelKnot   0  70  1.5
    ]
  , [
      rawLevelKnot   0  70  3.6
    , rawLevelKnot  60  30  3.2
    , rawLevelKnot -50 -10  2.8
    , rawLevelKnot  70 -20  1.4
    ]
  , [
      rawLevelKnot  70 -20  3
    , rawLevelKnot  40  80  2.5
    , rawLevelKnot  10 -60  2.1
    , rawLevelKnot -30  60  1.6
    , rawLevelKnot -70 -40  1.3
    ]
  , [
      rawLevelKnot -70 -40  2.6
    , rawLevelKnot -70  70  2.5
    , rawLevelKnot  70  70  2.4
    , rawLevelKnot  70 -70  2.3
    , rawLevelKnot -40 -70  2.2
    , rawLevelKnot -40  40  2.1
    , rawLevelKnot  40  40  2.0
    , rawLevelKnot  40 -40  1.8
    , rawLevelKnot -15 -40  1.6
    , rawLevelKnot -15  15  1.4
    , rawLevelKnot  10 -10  1.2
    ]
  ]


-- view configuration

manualText = "Guide the ball safely to its goal (green)."
respawnText = "Please return to the start (yellow)."
timeTextHeight = 7
timeTextPosY = 95
textHeight = 5
textPosY = -90


-- Inputs

type Input = { pos:(Int,Int), size:(Int,Int), clicked:Bool, delta:Float }

touchPosition : Touch.Touch -> (Int,Int)
touchPosition touch = (touch.x,touch.y)

touchPositions : Signal [(Int,Int)]
touchPositions = lift (\touches -> map touchPosition touches) Touch.touches

firstTouchPosition : Signal (Int,Int)
firstTouchPosition = keepIf (\tps -> length tps == 1) [(0,0)] touchPositions
                       |> lift (\tps -> head tps)

cursor = merge Mouse.position firstTouchPosition

ticker = lift (\t -> t / 1000) <| fps 10

input : Signal Input
input = (Input <~ cursor ~ Window.dimensions ~ Mouse.isClicked ~ ticker)

type TimestampedInput = Signal (Time, Input)
timestampedInput : TimestampedInput
timestampedInput = timestamp input


-- Model

type Positioned a = { a | x:Float, y:Float }
type Sized      a = { a | w:Float, h:Float }
type Point = Positioned {}
type Level = [Ball]

data State = Ready | Alive | Dead | Won
type Ball = Positioned { r:Float }
type LevelKnot = Ball

point : Float -> Float -> Point
point x y = {x=x, y=y}

ball : (Float,Float) -> Float -> Ball
ball (x,y) r = {x=x, y=y, r=r }
levelKnot = ball

scaleLevelWidth : Level -> Level
scaleLevelWidth = map (\k -> { k | r <- k.r * playerRadius })

generateLevel : RawLevel -> Level
generateLevel = scaleLevelWidth . map (uncurry levelKnot)

levels : [Level]
levels = map generateLevel levelsRaw

type Game = { state:State
            , player:Ball
            , levelsLeft:[Level]
            , lastRespawnTime:Float
            , oldTimeSum:Float
            , timeSum:Float }

defaultGame : Game
defaultGame =
  { state  = Dead
  , player = ball (0,0) playerRadius
  , levelsLeft = levels
  , lastRespawnTime = 0
  , oldTimeSum = 0
  , timeSum = 0 }


-- Updates

intPairToFloatPair : (Int,Int) -> (Float,Float)
intPairToFloatPair (a, b) = (toFloat a, toFloat b)

winPosToGamePos : (Int,Int) -> (Int,Int) -> (Float, Float)
winPosToGamePos pos size =
  let
    (winX, winY) = intPairToFloatPair pos
    (sizeX, sizeY) = intPairToFloatPair size
    (middleX, middleY) = (sizeX / 2, sizeY / 2)
    factor = gameScale size (gameWidth,gameHeight)
  in
    ((winX - middleX)  / factor, (middleY - winY) / factor)

dist {x,y} = sqrt (x^2 + y^2)

includes : Ball -> Ball -> Bool
includes outer inner =
  let
    centerDiff = point (abs (outer.x - inner.x)) (abs (outer.y - inner.y))
    centerDist = dist centerDiff
    radiiDiff = outer.r - inner.r
  in
    radiiDiff > centerDist

distToSegmentSquared : Point -> Point -> Point -> Float
distToSegmentSquared p v w =
  let
    dist2 : Point -> Point -> Float
    dist2 v w = (v.x - w.x)^2 + (v.y - w.y)^2
    l2 = dist2 v w
    t = ((p.x - v.x) * (w.x - v.x) + (p.y - v.y) * (w.y - v.y)) / l2
    nearestP = point (v.x + t * (w.x - v.x)) (v.y + t * (w.y - v.y))
  in
    if | l2 == 0 -> dist2 p v
       | t < 0 -> dist2 p v
       | t > 1 -> dist2 p w
       | otherwise -> dist2 p nearestP

distToSegment : Point -> Point -> Point -> Float
distToSegment p v w = sqrt <| distToSegmentSquared p v w


inLevel : Ball -> Level -> Bool
inLevel player level =
  let
    knotPairs = pairWise level
    knotToPoint k = point k.x k.y
  in
    any (\(k1, k2) -> distToSegment (knotToPoint player) (knotToPoint k1) (knotToPoint k2) < k1.r - player.r) knotPairs

-- todo:
-- cutting sharp should not kill you. ;-)

stepReady : Time -> Input -> Game -> Game
stepReady _ _ ({state,player,levelsLeft} as game) =
  let
    level = head levelsLeft
    atStart = head level `includes` player
    state' = if | atStart -> Alive
                | otherwise -> Ready
  in
    {game | state <- state' }

stepAlive : Time -> Input -> Game -> Game
stepAlive sysTime _ ({state,player,levelsLeft
                    ,lastRespawnTime,oldTimeSum,timeSum}
                    as game) =
  let
    level = head levelsLeft
    crash = not <| player `inLevel` level
    atGoal = last level `includes` player
    lastLevel = length levelsLeft == 1
    levelsLeft' = if | atGoal && not lastLevel -> tail levelsLeft
                     | otherwise -> levelsLeft
    state' = if | atGoal && lastLevel -> Won
                | crash -> Dead
                | otherwise -> Alive
    (oldTimeSum', lastRespawnTime') = if | state' == Dead -> (oldTimeSum + sysTime - lastRespawnTime, sysTime)
                                         | otherwise -> (oldTimeSum, lastRespawnTime)
    timeSumPrec = oldTimeSum' + sysTime - lastRespawnTime'
    timeSum' = (toFloat . round) (timeSumPrec / 100) / 10
  in
    {game | state <- state',
            levelsLeft <- levelsLeft',
            lastRespawnTime <- lastRespawnTime',
            oldTimeSum <- oldTimeSum',
            timeSum <- timeSum' }

stepDead : Time -> Input -> Game -> Game
stepDead sysTime _ ({state,player,levelsLeft,lastRespawnTime} as game) =
  let
    level = head levelsLeft
    atStart = head level `includes` player
    state' = if | atStart -> Alive
                | otherwise -> Dead
  in
    {game | state <- state',
            lastRespawnTime <- sysTime}

stepWon : Time -> Input -> Game -> Game
stepWon _ {clicked} ({state,player,levelsLeft} as game) =
  let
    (state',levelsLeft') = if | clicked -> (Dead,levels)
                             | otherwise -> (Won,levelsLeft)
  in
    {game | levelsLeft <- levelsLeft',
            state <- state'}

stepGame : Time -> Input -> Game -> Game
stepGame sysTime ({pos,size} as input) ({state,player} as game) =
  let
    (x,y) = winPosToGamePos pos size
    player' = {player | x <- x, y <- y}
    func = if | state == Alive -> stepAlive
              | state == Dead -> stepDead
              | state == Won -> stepWon
  in
    func sysTime input { game | player <- player' }

gameState : Signal Game
gameState = foldp (uncurry stepGame) defaultGame timestampedInput


-- Display

make : Color -> (Float, Float) -> Shape -> Form
make color (x,y) shape = shape |> filled color
                               |> move (x,y)

levelKnotsToPath : LevelKnot -> LevelKnot -> Path
levelKnotsToPath k1 k2 = path [(k1.x,k1.y),(k2.x,k2.y)]

displayLevelLine : Color -> LevelKnot -> LevelKnot -> Form
displayLevelLine col k1 k2 =
  let
    p = levelKnotsToPath k1 k2
    colStyle = solid col
    ls = { colStyle | width <- 2 * k1.r }
  in
    p |> traced ls


pairWise : [a] -> [(a,a)]
pairWise xs = case xs of
                [] -> []
                _  -> zip xs (tail xs)

noForm : Form
noForm = rect 0 0 |> filled (rgba 0 0 0 0)

displayLevel : Level -> State -> Form
displayLevel level state =
  let
    col = case state of
            Alive -> rgba 0 255 255 0.4
            Dead -> rgba 255 0 0 0.4
            Won -> rgba 0 255 0 0.4
    knotPairs = pairWise level
    knotCircle col k = circle k.r |> make col (k.x, k.y)
  in
    group <|
      map (uncurry <| displayLevelLine col) knotPairs
      ++ map (\(k1,k2) -> circle (k1.r) |> make col (k2.x, k2.y)) knotPairs
      ++ [(knotCircle yellow <| head level)]
      ++ [(knotCircle green <| last level)]

display : Game -> Form
display {state,player,levelsLeft,timeSum} =
  let
    level = head levelsLeft
    showText = case state of
                 Dead -> respawnText
                 Won -> "Congratulations! It took you "
                        ++ (show timeSum) ++ " seconds."
                        ++ " Click to improve. :)"
                 _ -> manualText
    textForm = txt (Text.height textHeight) showText
                 |> toForm |> move (0, textPosY)
    timeTextForm = txt (Text.height timeTextHeight) (show timeSum)
                 |> toForm |> move (0, timeTextPosY)

  in
    group
      [ rect gameWidth gameHeight |> filled darkBlue
      , displayLevel level state
      , circle player.r |> make lightGray (player.x, player.y)
      , textForm
      , timeTextForm
      ]

txt : (Text -> Text) -> String -> Element
txt f = text . f . monospace . Text.color lightBlue . toText

gameScale : (Int,Int) -> (Float,Float) -> Float
gameScale (winW, winH) (gameW,gameH) =
  min (toFloat winW / gameW) (toFloat winH / gameH)

displayFullScreen : (Int,Int) -> Game -> Element
displayFullScreen (w,h) game =
  let
    factor = gameScale (w,h) (gameWidth,gameHeight)
  in
    collage w h [ display game |> scale factor ]

main = lift2 displayFullScreen Window.dimensions <| dropRepeats gameState