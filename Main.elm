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

stepGame : Input -> Game -> Game
stepGame {pos,size} ({state,player,level} as game) =
  let
    (x,y) = winPosToGamePos pos size
    player' = {player | x <- x,
                        y <- y}
  in
    {game | player <- player'}

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
  in
    group <|
      map (uncurry <| displayLevelLine col) knotPairs
      ++ map (\k -> circle (2 * k.r) |> make col (k.x, k.y)) level

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