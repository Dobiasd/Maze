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
    , rawLevelKnot  30  70  4
    , rawLevelKnot   0 -70  4
    ]
  , [
      rawLevelKnot   0 -70  4
    , rawLevelKnot   0   0  3.6
    , rawLevelKnot  50   0  3.0
    , rawLevelKnot  50  40  2.4
    , rawLevelKnot   0  40  2
    , rawLevelKnot   0  70  1.4
    ]
  , [
      rawLevelKnot   0  70  3.6
    , rawLevelKnot  60  30  3.2
    , rawLevelKnot -50 -10  2.8
    , rawLevelKnot  70 -20  1.3
    ]
  , [
      rawLevelKnot  70 -20  3
    , rawLevelKnot  40  80  2.5
    , rawLevelKnot  10 -60  2.1
    , rawLevelKnot -30  60  1.6
    , rawLevelKnot -70 -40  1.2
    ]
  ]


-- view configuration

manualText = "Guide the ball safely to its goal (green)."
restartText = "Please return to the start (yellow)."
textHeight = 5
textFormPosY = -90

-- Inputs

type Input = { pos:(Int,Int), size:(Int,Int) }

touchPosition : Touch.Touch -> (Int,Int)
touchPosition touch = (touch.x,touch.y)

touchPositions : Signal [(Int,Int)]
touchPositions = lift (\touches -> map touchPosition touches) Touch.touches

firstTouchPosition : Signal (Int,Int)
firstTouchPosition = keepIf (\tps -> length tps == 1) [(0,0)] touchPositions
                       |> lift (\tps -> head tps)

cursor = merge Mouse.position firstTouchPosition

input : Signal Input
input = (Input <~ cursor ~ Window.dimensions)

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

type Game = { state:State, player:Ball, levelsLeft:[Level], time:Float }

defaultGame : Game
defaultGame =
  { state  = Dead,
    player = ball (0,0) playerRadius,
    levelsLeft = levels,
    time = 0 }


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


dist2 : Point -> Point -> Float
dist2 v w = (v.x - w.x)^2 + (v.y - w.y)^2

distToSegmentSquared : Point -> Point -> Point -> Float
distToSegmentSquared p v w =
  let
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
-- click to restart
-- points
-- time (display can be switched off) - use timestamp signal
-- cutting sharp should not kill you. ;-)


stepReady : Input -> Game -> Game
stepReady _ ({state,player,levelsLeft} as game) =
  let
    level = head levelsLeft
    atStart = head level `includes` player
    state' = if | atStart -> Alive
                | otherwise -> Ready
  in
    {game | state <- state',
            time <- 0 }

stepAlive : Input -> Game -> Game
stepAlive _ ({state,player,levelsLeft,time} as game) =
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
  in
    {game | state <- state',
            levelsLeft <- levelsLeft' }

stepDead : Input -> Game -> Game
stepDead _ ({state,player,levelsLeft} as game) =
  let
    level = head levelsLeft
    atStart = head level `includes` player
    state' = if | atStart -> Alive
                | otherwise -> Dead
  in
    {game | state <- state'}

stepWon : Input -> Game -> Game
stepWon _ ({state,player,levelsLeft} as game) =
  game

stepGame : Time -> Input -> Game -> Game
stepGame sysTime ({pos,size} as input) ({state,player} as game) =
  let
    (x,y) = winPosToGamePos pos size
    player' = {player | x <- x, y <- y}
    func = if | state == Alive -> stepAlive
              | state == Dead -> stepDead
              | state == Won -> stepWon
  in
    func input { game | player <- player' }

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
            --Alive -> blue
            --Dead -> red
            Alive -> rgba 0 255 255 0.4
            Dead -> rgba 255 0 0 0.4
            Won -> green
    knotPairs = pairWise level
    knotCircle col k = circle k.r |> make col (k.x, k.y)
  in
    group <|
      map (uncurry <| displayLevelLine col) knotPairs
      --++ map (\(k1,k2) -> circle (k1.r) |> make col (k2.x, k2.y)) knotPairs
      ++ [(knotCircle yellow <| head level)]
      ++ [(knotCircle green <| last level)]

display : Game -> Form
display {state,player,levelsLeft,time} =
  let
    level = head levelsLeft
    showText = if state == Alive then manualText else restartText
    textForm = txt (Text.height textHeight) showText
                 |> toForm |> move (0, textFormPosY)

  in
    group
      [ rect gameWidth gameHeight |> filled darkBlue
      , displayLevel level state
      , circle player.r |> make lightGray (player.x, player.y)
      , textForm
      --, round time |> asText |> toForm
      , time |> asText |> toForm
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