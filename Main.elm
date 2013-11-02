module Maze where

import Mouse
import Window


-- model configuration

(gameWidth,gameHeight) = (200,200)
(halfWidth,halfHeight) = (toFloat gameWidth / 2, toFloat gameHeight / 2)
playerRadius = 3

-- ((x,y),size from this knot on)
type RawLevel = [((Float,Float),Float)]
levelsRaw : [RawLevel]
levelsRaw =
  [
    [((  0,-70), 4)
   , ((  0,  0), 3.6)
   , (( 50,  0), 3.0)
   , (( 50, 40), 2.4)
   , ((  0, 40), 2)
   , ((  0, 80), 1.3)]
  ]


-- view configuration

manual = "Use your mouse to guide the ball safely to its goal."
startText = "Please return to the start."
backBlue = rgb  20  20  90
mazeBlue = rgb  40  40 170
mazeRed  = rgb 190  50  50


-- Inputs

type Input = { pos:(Int,Int), size:(Int,Int) }


input : Signal Input
input = (Input <~ Mouse.position ~ Window.dimensions)


-- Model

type Positioned a = { a | x:Float, y:Float }
type Sized      a = { a | w:Float, h:Float }
type Point = Positioned {}
type Level = [Ball]

data State = Alive | Dead
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

type Game = { state:State, player:Ball, level:Level }

defaultGame : Game
defaultGame =
  { state  = Dead,
    player = ball (0,0) playerRadius,
    level = head levels }


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


stepGame : Input -> Game -> Game
stepGame {pos,size} ({state,player,level} as game) =
  let
    (x,y) = winPosToGamePos pos size
    player' = {player | x <- x,
                        y <- y}
    start = head level
    atStart = start `includes` player
    crash = not <| player `inLevel` level
    state' = if crash then Dead else if atStart then Alive else state
  in
    {game | player <- player',
            state <- state'}

gameState : Signal Game
gameState = foldp stepGame defaultGame input


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


displayLevel : Level -> State -> Form
displayLevel level state =
  let
    col = if state == Alive then mazeBlue else mazeRed
    knotPairs = pairWise level
    knotCircle col k = circle k.r |> make col (k.x, k.y)
  in
    group <|
      map (uncurry <| displayLevelLine col) knotPairs
      ++ map (\(k1,k2) -> circle (k1.r) |> make col (k2.x, k2.y)) knotPairs
      ++ [(knotCircle green <| head level)]

display : Game -> Form
display {state,player,level} =
  group
    [ rect gameWidth gameHeight |> filled backBlue
    , displayLevel level state
    , circle player.r |> make lightGray (player.x, player.y)
    ]

gameScale : (Int,Int) -> (Float,Float) -> Float
gameScale (winW, winH) (gameW,gameH) =
  min (toFloat winW / gameW) (toFloat winH / gameH)

displayFullScreen : (Int,Int) -> Game -> Element
displayFullScreen (w,h) game =
  let
    factor = gameScale (w,h) (gameWidth,gameHeight)
  in
    collage w h [display game |> scale factor]

main = lift2 displayFullScreen Window.dimensions <| dropRepeats gameState