module Maze where

import Mouse
import Window


-- model configuration

playerRadius = 0.03

levelsRaw : [[(Float, Float)]]
levelsRaw =
  [
    [(1,1),(2,2)]
  ]


-- view configuration

manual = "Use your mouse to guide the ball safely to the goal."
backBlue = rgb 20 20 90
mazeBlue = rgb 40 40 170
mazeRed = rgb 190 50 50


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

mousePosToGamePos : (Int,Int) -> (Int,Int) -> (Float, Float)
mousePosToGamePos pos size =
  let
    (mouseX, mouseY) = intPairToFloatPair pos
    (sizeX, sizeY) = intPairToFloatPair size
    (middleX, middleY) = (sizeX / 2, sizeY / 2)
  in
    (mouseX - middleX, middleY - mouseY)

stepGame : Input -> Game -> Game
stepGame {pos,size} ({state,player,level} as game) =
  let
    (x,y) = mousePosToGamePos pos size
    player' = {player | x <- x,
                        y <- y}
  in
    {game | player <- player'}

gameState : Signal Game
gameState = foldp stepGame defaultGame input


-- Display

make : Color -> Positioned a -> Shape -> Form
make color obj shape = shape |> filled color
                             |> move (obj.x,obj.y)

displayLevel : Level -> (Float,Float) -> State -> Element
displayLevel level (w,h) state =
  let
    color = if state == Alive then mazeBlue else mazeRed
  in
    spacer 1 1

display : (Int,Int) -> Game -> Element
display (winWidth,winHeight) {state,player,level} =
  let
    w = toFloat winWidth
    h = toFloat winHeight
    edgeLen = min w h
    r = edgeLen * player.r
  in
    collage winWidth winHeight
      [ rect w h |> filled backBlue
      , displayLevel level (w,h) state |> toForm
      , circle r |> make lightGray player
      ]

main = lift2 display Window.dimensions <| dropRepeats gameState