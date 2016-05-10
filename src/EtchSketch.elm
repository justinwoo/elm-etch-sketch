module Main exposing (..)

import Html.App exposing (program)
import Html exposing (Html, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Keyboard exposing (KeyCode, downs)
import Svg exposing (svg, rect)
import Svg.Attributes as SvgAttrs


type Direction
  = Up
  | Down
  | Left
  | Right


type Coords
  = Coords Int Int


type alias Model =
  { cursor : Coords
  , points : List Coords
  , width : Int
  , height : Int
  , increment : Int
  }


type Msg
  = MoveCursor Direction
  | ClearScreen
  | NoOp


init : Model
init =
  { cursor = Coords 0 0
  , points = []
  , width = 800
  , height = 600
  , increment = 10
  }


isInvalidPoint : Model -> Coords -> Bool
isInvalidPoint model coords =
  case coords of
    Coords x y ->
      if x < 0 then
        True
      else if y < 0 then
        True
      else if (model.increment * (x + 1)) > model.width then
        True
      else if (model.increment * (y + 1)) > model.height then
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


moveCursor : Direction -> Model -> Model
moveCursor direction model =
  case model.cursor of
    Coords x y ->
      let
        points' =
          insertPoint model.cursor model.points

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

      in
        if isInvalidPoint model cursor' then
          model
        else
          { model | cursor = cursor', points = points' }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ClearScreen ->
      ( { model | points = [] }, Cmd.none )
    MoveCursor direction ->
      ( moveCursor direction model, Cmd.none )
    NoOp ->
      ( model, Cmd.none )


point : Int -> String -> Coords -> Html Msg
point increment subkey (Coords x y) =
  rect
    [ SvgAttrs.width <| toString increment
    , SvgAttrs.height <| toString increment
    , SvgAttrs.x <| toString <| x * increment
    , SvgAttrs.y <| toString <| y * increment
    ]
    []


view : Model -> Html Msg
view model =
  let
    point' =
      point model.increment

    cursor =
      point' "cursor" model.cursor

    points =
      List.map (point' "point") model.points
  in
    div
      []
      [ div
          []
          [ button
              [ onClick ClearScreen ]
              [ text "Clear" ]
          ]
      , div
          []
          [ svg
              [ style [ ( "border", "1px solid black" ) ]
              , SvgAttrs.width <| toString model.width
              , SvgAttrs.height <| toString model.height
              ]
              <| cursor
              :: points
          ]
      ]


getKeyDirection : KeyCode -> Msg
getKeyDirection keyCode =
  case keyCode of
    38 ->
      MoveCursor Up

    40 ->
      MoveCursor Down

    37 ->
      MoveCursor Left

    39 ->
      MoveCursor Right

    _ ->
      NoOp


main =
  program
    { init = ( init, Cmd.none )
    , update = update
    , view = view
    , subscriptions = (\_ -> downs getKeyDirection)
    }
