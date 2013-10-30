module Maze where

import Mouse
import Window


-- model configuration

playerRadius = 0.03

levelsRaw : [[(Float, Float)]]
levelsRaw =
  [
    [(0,-80),(0,0),(50,0),(50,40),(0,40),(0,80)]
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
type Level = [Point]

data State = Alive | Dead
type Ball = Positioned { r:Float }

point : Float -> Float -> Point
point x y = {x=x, y=y}

levels : [Level]
levels =
  let
    convertLevel = map (\(x,y) -> point (x / 100) (y / 100))
  in
    map convertLevel levelsRaw

ball : Float -> Float -> Float -> Ball
ball x y r = {x=x, y=y, r=r }

type Game = { state:State, player:Ball, level:Level }

defaultGame : Game
defaultGame =
  { state  = Dead,
    player = ball 0 0 playerRadius,
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
  in
    ((winX - middleX) / middleX, (middleY - winY) / middleY)

gamePosToWinPos : Positioned a -> (Float,Float) -> (Float, Float)
gamePosToWinPos {x,y} (sizeX,sizeY) =
  let
    (middleX, middleY) = (sizeX / 2, sizeY / 2)
  in
    (x * middleX, y * middleY)

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

displayLevel : Level -> (Float,Float) -> State -> Element
displayLevel level (w,h) state =
  let
    winLevel = map (gamePosToWinPos)
    --outline |> traced (dashed blue)
    color = if state == Alive then mazeBlue else mazeRed
  in
    spacer 1 1

display : (Int,Int) -> Game -> Element
display (winWidth,winHeight) {state,player,level} =
  let
    edgeInt = min winWidth winHeight
    edge = toFloat edgeInt
    r = edge * player.r
    winPos = gamePosToWinPos player (toFloat winWidth, toFloat winHeight)
  in
    container winWidth winHeight middle <| collage edgeInt edgeInt
      [ rect edge edge |> filled backBlue
      , displayLevel level (edge,edge) state |> toForm
      , circle r |> make lightGray winPos
      ]

main = lift2 display Window.dimensions <| dropRepeats gameState