module Maze where

import Mouse
import Window


-- model configuration

type Positioned a = { a | x:Float, y:Float }
type Sized      a = { a | w:Float, h:Float }
type Point = Positioned {}

--point : (Float, Float) -> Point
--point (x, y) = {x=x, y=y}

point : Float -> Float -> Point
point x y = {x=x, y=y}

playerRadius = 7
type Level = [Point]

levelsRaw : [[(Float, Float)]]
levelsRaw =
  [
    [(1,1),(2,2)]
  ]

levels : [Level]
levels =
  let
    convertLevel = map (\(x,y) -> point (x / 100) (y / 100))
  in
    map convertLevel levelsRaw


-- view configuration

manual = "Use your mouse to guide the ball safely to the goal."
mazeBlue = rgb 40 40 180


-- Inputs

type Input = { pos:(Int,Int), size:(Int,Int) }


input : Signal Input
input = (Input <~ Mouse.position ~ Window.dimensions)


-- Model

data State = Alive | Dead

type Ball = Positioned { r:Float }

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

display : (Int,Int) -> Game -> Element
display (winWidth,winHeight) {state,player,level} =
  let
    w = toFloat winWidth
    h = toFloat winHeight
  in
    collage winWidth winHeight
      [ rect w h |> filled mazeBlue
      , circle player.r |> make lightGray player
      ]

main = lift2 display Window.dimensions <| dropRepeats gameState