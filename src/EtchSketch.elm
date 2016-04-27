module Main (..) where

import Char exposing (KeyCode)
import Html exposing (Html, div, button, text)
import Html.Attributes exposing (key, style)
import Html.Events exposing (onClick)
import Svg exposing (svg, rect)
import Svg.Attributes as SvgAttrs


type Direction
  = Up
  | Down
  | Left
  | Right
  | None


type Coords
  = Coords Int Int


type alias State =
  { cursor : Coords
  , points : List Coords
  , width : Int
  , height : Int
  , increment : Int
  }


initState : State
initState =
  { cursor = Coords 0 0
  , points = []
  , width = 800
  , height = 600
  , increment = 10
  }


isInvalidPoint : State -> Coords -> Bool
isInvalidPoint state coords =
  case coords of
    Coords x y ->
      if x < 0 then
        True
      else if y < 0 then
        True
      else if (state.increment * (x + 1)) > state.width then
        True
      else if (state.increment * (y + 1)) > state.height then
        True
      else
        False


insertPoint : Coords -> List Coords -> List Coords
insertPoint point points =
  case List.member point points of
    True ->
      points

    False ->
      point :: points


moveCursor : Direction -> State -> State
moveCursor direction state =
  case state.cursor of
    Coords x y ->
      let
        points' =
          insertPoint state.cursor state.points

        cursor' =
          case direction of
            Up ->
              Coords x (y - 1)

            Down ->
              Coords x (y + 1)

            Left ->
              Coords (x - 1) y

            Right ->
              Coords (x + 1) y

            None ->
              state.cursor
      in
        if isInvalidPoint state cursor' then
          state
        else
          { state | cursor = cursor', points = points' }


wipeScreen : a -> State -> State
wipeScreen _ state =
  { state | points = [] }


point : Int -> String -> Coords -> Html
point increment subkey (Coords x y) =
  rect
    [ key <| subkey ++ toString x ++ "," ++ toString y
    , SvgAttrs.width <| toString increment
    , SvgAttrs.height <| toString increment
    , SvgAttrs.x <| toString <| x * increment
    , SvgAttrs.y <| toString <| y * increment
    ]
    []


view : State -> Html
view state =
  let
    point' =
      point state.increment

    cursor =
      point' "cursor" state.cursor

    points =
      List.map (point' "point") state.points
  in
    div
      []
      [ div
          []
          [ button
              [ onClick screenWipes.address True ]
              [ text "Clear" ]
          ]
      , div
          []
          [ svg
              [ style [ ( "border", "1px solid black" ) ]
              , SvgAttrs.width <| toString state.width
              , SvgAttrs.height <| toString state.height
              ]
              <| cursor
              :: points
          ]
      ]


getKeyDirection : KeyCode -> Direction
getKeyDirection keyCode =
  case keyCode of
    38 ->
      Up

    40 ->
      Down

    37 ->
      Left

    39 ->
      Right

    _ ->
      None


port keyboard : Signal KeyCode
keyDirections : Signal Direction
keyDirections =
  Signal.map getKeyDirection keyboard


screenWipes : Signal.Mailbox Bool
screenWipes =
  Signal.mailbox True


projects : Signal (State -> State)
projects =
  Signal.mergeMany
    [ Signal.map moveCursor keyDirections
    , Signal.map wipeScreen screenWipes.signal
    ]


main : Signal Html
main =
  Signal.map view <| Signal.foldp (<|) initState projects
