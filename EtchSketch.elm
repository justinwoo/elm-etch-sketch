import Color exposing (Color, rgb)
import Graphics.Collage exposing (Form, collage, rect, filled, move)
import Graphics.Element exposing (Element)
import Keyboard
import Window

cellSize : Float
cellSize = 16

-- MODEL

-- 2-dimensional coordinates here
type alias Coordinates = (Int, Int)

-- directions the cursor can go
type Direction = Up | Down | Left | Right | None

-- value of arrow keypresses
type alias Keys = { x:Int, y:Int }

type alias Model =
  {
    cursor : Coordinates,
    points : List Coordinates
  }

state : Model
state =
  {
    cursor = (0, 0),
    points = []
  }


-- UPDATE

update : Direction -> Model -> Model
update direction state =
  state
    |> paintTrail
    |> moveCursor direction

paintTrail : Model -> Model
paintTrail state =
  if List.member state.cursor state.points
     then state
     else { state | points <- state.cursor :: state.points }

moveCursor : Direction -> Model -> Model
moveCursor direction state =
  let
    (x, y) = state.cursor
  in
    case direction of
      Up -> { state | cursor <- (x, y + 1) }
      Down -> { state | cursor <- (x, y - 1) }
      Right -> { state | cursor <- (x + 1, y) }
      Left -> { state | cursor <- (x - 1, y) }
      _ -> state


-- VIEW

getDrawPoint : Color -> Coordinates -> Form
getDrawPoint color coords =
  let
    (x, y) = coords
  in
    rect cellSize cellSize
      |> filled color
      |> move (toFloat x * cellSize, toFloat y * cellSize)

myView: (Int, Int) -> Model -> Element
myView (w, h) state =
  let
    points =
      List.map (getDrawPoint(rgb 50 50 50)) state.points
    cursor =
      getDrawPoint(rgb 150 150 150) state.cursor
  in
    collage w h
      (List.append points [cursor])

-- SIGNALS

main : Signal Element
main =
  Signal.map2 myView Window.dimensions (Signal.foldp update state myInput)

getKeyDirection : Keys -> Direction
getKeyDirection keys =
  if | keys.y > 0 -> Up
     | keys.y < 0 -> Down
     | keys.x > 0 -> Right
     | keys.x < 0 -> Left
     | otherwise -> None

myInput : Signal Direction
myInput =
  Signal.map getKeyDirection (Signal.merge Keyboard.arrows Keyboard.wasd)
