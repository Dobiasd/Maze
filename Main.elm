module Maze where

{-| A simple maze game requiring mouse or touch precision under time presure.

The game and the measured time begins when the player moves
to the marked start position. It is paused if the player crashed into a wall.
He then has to go back the the start of the current level.

If the level goal is reached, the next level is started automatically.
After finishing the last level, the summed up time inversly
indicates the players performance. ;-)
-}

import Mouse
import Touch
import Window

-- /---------------------\
-- | model configuration |
-- \---------------------/

{-| The game field extends from -100 to +100 in x and y coordinates. -}
(gameWidth,gameHeight) = (200,200)

{-| Since the player is ball shaped, this is his main property. -}
playerRadius = 5

{-| Levels are designed as a path of knots.
Every knot consists of position and a radius, and thus is the same as a ball.
The radius (given as a factor to the players radius to the constructor)
determines the half of the width of the line segment (connection of two knots)
following this know in the level path. -}

{-| To provide a smooth gaming experience with fluent transitions
between two consecutive levels, the levels should be designed such
that the last point (l) of level n equals the first point (f) of level n+1.
radius of f <= radius f. -}
levels : [Level]
levels =
  [
    [
      levelKnot -70 -65  4
    , levelKnot  30  70  3.6
    , levelKnot   0 -65  2.3
    ]
  , [
      levelKnot   0 -65  4
    , levelKnot   0   0  3.7
    , levelKnot  50   0  3.3
    , levelKnot  50  40  2.7
    , levelKnot   0  40  2.3
    , levelKnot   0  70  1.9
    ]
  , [
      levelKnot   0  70  3.7
    , levelKnot  60  30  3.4
    , levelKnot -50 -10  2.8
    , levelKnot  70 -20  1.7
    ]
  , [
      levelKnot  70 -20  3
    , levelKnot  40  80  2.5
    , levelKnot  10 -60  2.1
    , levelKnot -30  60  1.9
    , levelKnot -70 -40  1.6
    ]
  , [
      levelKnot -70 -40  2.5
    , levelKnot -70  70  2.4
    , levelKnot  70  70  2.3
    , levelKnot  70 -70  2.2
    , levelKnot -40 -70  2.1
    , levelKnot -40  40  2.0
    , levelKnot  40  40  1.9
    , levelKnot  40 -40  1.8
    , levelKnot -15 -40  1.7
    , levelKnot -15  15  1.6
    , levelKnot  10 -10  1.5
    ]
  ]


-- /--------------------\
-- | view configuration |
-- \--------------------/

manualText = "Guide the ball safely to its goal (green)."
respawnText = "Please go to the start (yellow) to begin/respawn."
timeTextHeight = 7
timeTextPosY = 95
textHeight = 5
textPosY = -90
aliveColor = rgba   0 255 255 0.4
deadColor  = rgba 255   0   0 0.4
wonColor   = rgba   0 255   0 0.4


-- /--------\
-- | inputs |
-- \--------/

{-| Extract the latest coordinates from a touch. -}
touchPosition : Touch.Touch -> (Int,Int)
touchPosition touch = (touch.x,touch.y)

{-| Extract all the latest touch positions. -}
touchPositions : Signal [(Int,Int)]
touchPositions = lift (\touches -> map touchPosition touches) Touch.touches

{-| If exactly one touch is present, use its coordinates. (0,0) otherwise. -}
firstTouchPosition : Signal (Int,Int)
firstTouchPosition =
  let f tps = if length tps == 1 then head tps else (0,0)
  in lift f touchPositions

{-| If the user touching two positions at once? -}
multiTouch : Signal Bool
multiTouch = lift (\touches -> length touches > 1) Touch.touches

{-| Mouse clicks and multi touches count as clicks. -}
clicked : Signal Bool
clicked = lift2 (||) Mouse.isClicked multiTouch

{-| The player and use his mouse or touch screen. -}
cursor : Signal (Int,Int)
cursor = merge Mouse.position firstTouchPosition

{-| We want the time to update every 100 milliseconds if possible. -}
ticker = lift (\t -> t / 1000) <| fps 10

{-| Relevant things that can change are:
- the position of the player on the screen
- the size of the window
- did the user click?
- did the time tick?
The value of delta is not used. It is just needed to trigger an update. -}
type Input = { pos:(Int,Int), size:(Int,Int), clicked:Bool, delta:Float }

input : Signal Input
input = (Input <~ cursor ~ Window.dimensions ~ clicked ~ ticker)

{-| Every input gets a timestamp, so our performance stop watch
can work with maximum precision. -}
type TimestampedInput = Signal (Time, Input)
timestampedInput : TimestampedInput
timestampedInput = timestamp input


-- /-------\
-- | model |
-- \-------/

type Positioned a = { a | x:Float, y:Float }
type Point = Positioned {}
type Line = (Point,Point)
type Level = [Ball]

data State = Alive | Dead | Won
type Ball = Positioned { r:Float }
type LevelKnot = Ball

point : Float -> Float -> Point
point x y = {x=x, y=y}

line : Positioned a -> Positioned a -> Line
line p1 p2 = (point p1.x p1.y, point p2.x p2.y)

ball : (Float,Float) -> Float -> Ball
ball (x,y) r = {x=x, y=y, r=r }

levelKnot : Float -> Float -> Float -> LevelKnot
levelKnot x y r = {x=x, y=y, r=r*playerRadius }

type Game = { state:State
            , player:Ball

-- Contains the levels yet to be finished by the player.
-- If the last level is solved, it will remain here,
-- so this list will never get empty.
            , levelsLeft:[Level]

-- When was the last time the player was dead and then entered the start.
            , lastRespawnTime:Float

-- The sum of all past time spans (respawn to crash) without the current
            , oldTimeSum:Float

-- The time including all past time spans and the currently running.
-- It is recalculated at every update and only used for the display.
            , timeSum:Float }


defaultGame : Game
defaultGame =
  { state  = Dead
  , player = ball (0,0) playerRadius
  , levelsLeft = levels
  , lastRespawnTime = 0
  , oldTimeSum = 0
  , timeSum = 0 }


-- /---------\
-- | updates |
-- \---------/

{-| Since the game is always scaled maximally into the window
(keeping its aspect ratio), the mouse and touch positions
have to be converted to game positions. -}
winPosToGamePos : (Int,Int) -> (Int,Int) -> (Float, Float)
winPosToGamePos pos size =
  let
    intPairToFloatPair (a, b) = (toFloat a, toFloat b)
    (winX, winY) = intPairToFloatPair pos
    (sizeX, sizeY) = intPairToFloatPair size
    (middleX, middleY) = (sizeX / 2, sizeY / 2)
    factor = gameScale size (gameWidth,gameHeight)
  in
    ((winX - middleX) / factor, (middleY - winY) / factor)

{-| Calculate factor by which the game is scaled visually onto the screen. -}
gameScale : (Int,Int) -> (Float,Float) -> Float
gameScale (winW, winH) (gameW,gameH) =
  min (toFloat winW / gameW) (toFloat winH / gameH)

{-| Euclidian 2d distance. -}
dist : Positioned a -> Float
dist {x,y} = sqrt (x^2 + y^2)

{-| Does outer include inner? -}
includes : Ball -> Ball -> Bool
includes outer inner =
  let
    centerDiff = point (abs (outer.x - inner.x)) (abs (outer.y - inner.y))
    centerDist = dist centerDiff
    radiiDiff = outer.r - inner.r
  in
    radiiDiff > centerDist

{-| Does outer include inner? -}
-- source: http://stackoverflow.com/questions/849211
distToSegmentSquared : Positioned a -> Positioned b -> Positioned c -> Float
distToSegmentSquared p v w =
  let
    dist2 v w = (v.x - w.x)^2 + (v.y - w.y)^2
    l2 = dist2 v w -- i.e. |w-v|^2 - avoid a sqrt

    -- Consider the line extending the segment,
    -- parameterized as v + t (w - v).
    -- We find projection of point p onto the line.
    -- It falls where t = [(p-v) . (w-v)] / |w-v|^2
    t = ((p.x - v.x) * (w.x - v.x) + (p.y - v.y) * (w.y - v.y)) / l2

    -- Projection falls on the segment
    nearestP = point (v.x + t * (w.x - v.x)) (v.y + t * (w.y - v.y))
  in
    if | l2 == 0 -> dist2 p v -- v == w case
       | t < 0 -> dist2 p v -- Beyond the 'v' end of the segment
       | t > 1 -> dist2 p w -- Beyond the 'w' end of the segment
       | otherwise -> dist2 p nearestP

{-| Minimum distance between line segment vw and point p. -}
distToSegment : Positioned a -> Positioned b -> Positioned c -> Float
distToSegment p v w = sqrt <| distToSegmentSquared p v w

{-| Intersection of the two lines.
Nothing if lines are parallel or coincide. -}
intersectLineLine : Line -> Line -> Maybe Point
intersectLineLine (p1,p2) (p3,p4) =
  let
    dx12  = p1.x - p2.x
    dx34  = p3.x - p4.x
    dy12  = p1.y - p2.y
    dy34  = p3.y - p4.y
    den = dx12 * dy34  - dy12 * dx34
  in
    if den == 0 then Nothing else
      let
        det12 = p1.x*p2.y - p1.y*p2.x
        det34 = p3.x*p4.y - p3.y*p4.x
        numx  = det12 * dx34 - dx12 * det34
        numy  = det12 * dy34 - dy12 * det34
          in
            Just <| point (numx / den) (numy / den)

{-| Ball fully covered by level segment? -}
inSegment : Ball -> (LevelKnot,LevelKnot) -> Bool
inSegment player (k1,k2) = distToSegment player k1 k2 < k1.r - player.r

{-| Ball overlapping with level segment? -}
touchingSegment : Ball -> (LevelKnot,LevelKnot) -> Bool
touchingSegment player (k1,k2) = distToSegment player k1 k2 < k1.r + player.r

{-| Vector substraction: v2 - v1 -}
diffVec : Positioned a -> Positioned b -> Point
diffVec v1 v2 = point (v2.x - v1.x) (v2.y - v1.y)

{-| Vector addition. -}
movePoint : Positioned a -> Point -> Positioned a
movePoint p v = {p | x <- p.x + v.x, y <- p.y + v.y}

{-| Scale vector. -}
scaleVec : Float -> Positioned a -> Positioned a
scaleVec s v = {v | x <- v.x * s, y <- v.y * s}

{-| Normalize vector to lenght 1. -}
normVec : Positioned a -> Positioned a
normVec v =
  let l = dist v
  in {v | x <- v.x / l, y <- v.y / l}

{-| Return the two possible perpendicular vectors of v. -}
perpendiculars : Point -> (Point, Point)
perpendiculars v = (point -v.y v.x, point v.y -v.x)

{-| Return the two possible parallel lines to the
line l going through p1 and p2 with a given distance to l. -}
parallelLines : Positioned a -> Positioned a -> Float -> (Line,Line)
parallelLines p1 p2 dist =
  let
    diff = diffVec p1 p2 |> normVec |> scaleVec dist
    (pp1,pp2) = perpendiculars diff
    p1a = movePoint p1 pp1
    p2a = movePoint p2 pp1
    p1b = movePoint p1 pp2
    p2b = movePoint p2 pp2
    l1 = line p1a p2a
    l2 = line p1b p2b
  in
    (l1, l2)



-- Not it gets a little tricky. ;-)
-- Inside bends of a link of to segments (l1 and l2) provide situations
-- in which the ball is neither fully inside l1 nor fully inside l2,
-- but it touches boths and with their union l1 and l2 cover the ball
-- completely.
--
-- Without handling this edge case, a collision would occur when the player is
-- going sharply through the edges on the inside, even though it should not.
--
-- Version would be to convert the whole level into a big polygon
-- and check if the ball is inside of it. This would give more flexibility
-- in the level shape, but since the levels are at the moment
-- just a bar path, I feel this would be kind of a overkill solution.
--
-- Another (even more ugly) version would be to not check the
-- players ball as a whole, but to interpret it as a list of
-- many (m) of its surface points, and check if they all are inside one
-- of the (n) level segments. This would of course work and not be too
-- complicated to implement, but it would lift the algorithm
-- into a higher complexity class (O(n) -> O(n*m)), so I did not want to
-- use this solution either.
--
-- Luckily there is a third possibility. :-)
-- We have to check if the the following three conditions are fulfilled:
-- 1) The ball is touching both segments involved in the bend.
--    (If it not even touching both, we are definitely outside the level.)
-- 2) The ball is inside the triangle provided by the two segments.
--    (This prevents extension of the space at the outer bend.)
-- 3) The ball does not touch the point s.
--    s is the line intersection of the two inside borders of the segments.
{-| Is the player inside the bend between the two adjecent
segments (k1,k2) and (k2,k3)? -}
inInsideBend : Ball -> (LevelKnot,LevelKnot,LevelKnot) -> Bool
inInsideBend player (k1,k2,k3) =
  let
    (b1A, b1B) = parallelLines k1 k2 k1.r -- borders of the first segment
    (b2A, b2B) = parallelLines k2 k3 k2.r -- borders of the second
    s1 = intersectLineLine b1A b2A -- the four possible intersections
    s2 = intersectLineLine b1B b2A -- of the two line pairs above.
    s3 = intersectLineLine b1A b2B
    s4 = intersectLineLine b1B b2B
    touchingSegment1 = touchingSegment player (k1,k2)
    touchingSegment2 = touchingSegment player (k2,k3)
    touchingBothSegments = touchingSegment1 && touchingSegment2
    ss = justs [s1,s2,s3,s4] -- extract the possible intersections
    triangle = (k1,k2,k3) -- the triangle spanned by the two segments
    -- Since the following line is only evaluated if there is at least
    -- one element in ss, the function as a whole is still total.
    s = filter (inTriangle triangle) ss |> head
    touchingS = dist {x = s.x - player.x, y = s.y - player.y} < player.r
  in
    if isEmpty ss then False else
      touchingBothSegments && inTriangle triangle player && not touchingS

{-| Point inside triangle? -}
-- source: http://stackoverflow.com/questions/2049582
inTriangle : (Positioned a, Positioned a, Positioned a)
  -> Positioned b -> Bool
inTriangle (v1,v2,v3) pt =
  let
    sign p1 p2 p3 = (p1.x-p3.x) * (p2.y-p3.y) - (p2.x-p3.x) * (p1.y-p3.y)
    b1 = sign pt v1 v2 < 0
    b2 = sign pt v2 v3 < 0
    b3 = sign pt v3 v1 < 0
  in
    (b1 == b2) && (b2 == b3)

{-| [1,2,3,4] -> [(1,2),(2,3),(3,4)] -}
pairWise : [a] -> [(a,a)]
pairWise xs = case xs of
                [] -> []
                _  -> zip xs (tail xs)

{-| [a,b] [1,2] [x,y] -> [(a,1,x),(b,2,y)] -}
zip3 : [a] -> [b] -> [c] -> [(a,b,c)]
zip3 xs ys zs = case (xs, ys, zs) of
                  (x::xs', y::ys', z::zs') -> (x,y,z) :: zip3 xs' ys' zs'
                  otherwise -> []

{-| [1,2,3,4,5] -> [(1,2,3),(2,3,4),(3,4,5)] -}
tripleWise : [a] -> [(a,a,a)]
tripleWise xs = case xs of
                  [] -> []
                  _::[] -> []
                  _  -> zip3 xs (tail xs) (tail (tail xs))

{-| Is the player inside the level or did a crash occur? -}
inLevel : Ball -> Level -> Bool
inLevel player level =
  let
    knotPairs = pairWise level -- the segments
    knotTriples = tripleWise level -- the links of two adjecent segments
    completelyInOneSegment = any (inSegment player) knotPairs
    inABend = any (inInsideBend player) knotTriples
  in
    completelyInOneSegment || inABend


gameState : Signal Game
gameState = foldp (uncurry stepGame) defaultGame timestampedInput

{-| Update player position and
dispatch according to the current game state. -}
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

{-| Step game when player is alive. -}
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
    (oldTimeSum', lastRespawnTime') =
      if | state' == Dead -> (oldTimeSum + sysTime - lastRespawnTime, sysTime)
         | otherwise -> (oldTimeSum, lastRespawnTime)
    timeSumPrec = oldTimeSum' + sysTime - lastRespawnTime'
    timeSum' = (toFloat . round) (timeSumPrec / 100) / 10
  in
    {game | state <- state',
            levelsLeft <- levelsLeft',
            lastRespawnTime <- lastRespawnTime',
            oldTimeSum <- oldTimeSum',
            timeSum <- timeSum' }

{-| Step game when player is dead. -}
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

{-| Step game when the game is finished. -}
stepWon : Time -> Input -> Game -> Game
stepWon sysTime {clicked} ({state,player,levelsLeft,timeSum} as game) =
  let
    (state',levelsLeft') = if | clicked -> (Dead,levels)
                              | otherwise -> (Won,levelsLeft)
    timeSum' = if state' == Dead then 0 else timeSum
  in
    {game | levelsLeft <- levelsLeft',
            state <- state',
            lastRespawnTime <- sysTime,
            oldTimeSum <- 0,
            timeSum <- timeSum'}


-- /---------\
-- | display |
-- \---------/

{-| Take a shape, give it a color and move it.. -}
make : Color -> (Float, Float) -> Shape -> Form
make color (x,y) shape = shape |> filled color |> move (x,y)

{-| Convert level knots to a displayable path. -}
levelKnotsToPath : LevelKnot -> LevelKnot -> Path
levelKnotsToPath k1 k2 = path [(k1.x,k1.y),(k2.x,k2.y)]

{-| Show level beam with a specific color. -}
displayLevelLine : Color -> Bool -> LevelKnot -> LevelKnot -> Form
displayLevelLine col thin k1 k2 =
  let
    p = levelKnotsToPath k1 k2
    colStyle = solid col
    ls = { colStyle | width <- if thin then 1 else 2 * k1.r }
  in
    p |> traced ls

{-| Show a complete level. -}
displayLevel : Level -> State -> Form
displayLevel level state =
  let
    col = case state of
            Alive -> aliveColor
            Dead  -> deadColor
            Won   -> wonColor
    knotPairs = pairWise level
    knotCircle col k = circle k.r |> make col (k.x, k.y)
  in
    group <|
      map (uncurry <| displayLevelLine col False) knotPairs -- wide beams
      ++ map (uncurry <| displayLevelLine col True) knotPairs -- beams centers
      ++ map (\(k1,k2) -> circle (k1.r) |> make col (k2.x, k2.y)) knotPairs
      ++ [(knotCircle yellow <| head level)] -- start
      ++ [(knotCircle green <| last level)] -- goal

{-| Render text using a given transformation function. -}
txt : (Text -> Text) -> String -> Element
txt f = text . f . monospace . Text.color lightBlue . toText

{-| Draw game into a form with size (gameWidth,gameHeight). -}
display : Game -> Form
display {state,player,levelsLeft,timeSum} =
  let
    level = head levelsLeft
    showText = case state of
                 Dead -> respawnText
                 Won -> "Yeah! Just " ++ (show timeSum) ++ " seconds."
                        ++ " Click (or multi touch) to improve. :)"
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

{-| Draw game maximized into the window. -}
displayFullScreen : (Int,Int) -> Game -> Element
displayFullScreen (w,h) game =
  let
    factor = gameScale (w,h) (gameWidth,gameHeight)
  in
    collage w h [ display game |> scale factor ]

main = lift2 displayFullScreen Window.dimensions <| dropRepeats gameState