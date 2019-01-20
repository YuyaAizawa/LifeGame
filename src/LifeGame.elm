module LifeGame exposing (..)

import Browser
import Array2D exposing (Array2D)
import List
import Maybe
import Html exposing (Html, div, input, label, input, span, text)
import Html.Attributes exposing (type_, value, style)
import Html.Events exposing (onClick, onMouseDown, onMouseUp, onMouseOver)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Svg exposing (svg, circle)
import Svg.Attributes exposing (width, height, viewBox, class, cx, cy, r, fill, stroke)
import Time

main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL

type alias Model =
  { field : Array2D Life
  , mouseOverAction : MouseOverAction
  , auto : Bool
  }

type Life
  = Alive Int
  | Dead

flip life =
  case life of
    Alive _ -> Dead
    Dead -> Alive 0

type MouseOverAction
  = NoAction
  | AddLife
  | RemoveLife

init : {width : Int, height : Int} -> (Model, Cmd msg)
init {width, height} =
    ( { field = initField width height
      , mouseOverAction = NoAction
      , auto = False
      }
    , Cmd.none
    )

initField width height =
  Array2D.initialize width height (always (always Dead))



-- UPDATE

type Msg
  = Nop
  | Step
  | MouseDown Int Int Life
  | MouseUp
  | MouseOver Int Int
  | ToggleAuto
  | Reset

update msg model =
  let
    nextModel =
      case msg of
        Nop -> model

        Step ->
          { model | field =
            nextGen model.field }

        MouseDown x y life ->
          { model
          | mouseOverAction =
              case life of
                Alive _ ->
                  RemoveLife
                Dead ->
                  AddLife
          , field = Array2D.set x y (flip life) model.field
          }

        MouseUp ->
          { model | mouseOverAction = NoAction }

        MouseOver x y ->
          case model.mouseOverAction of
            NoAction ->
              model
            AddLife ->
              addLife x y model
            RemoveLife ->
              removeLife x y model

        ToggleAuto ->
          { model | auto = not model.auto }

        Reset ->
          let
            height = model.field |> Array2D.height
            width = model.field |> Array2D.width
          in
            { model | field = initField width height }
  in
    (nextModel, Cmd.none)

nextGen field =
  let
    neighbors x y =
      [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
      ,(x - 1, y    ),             (x + 1, y    )
      ,(x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

    getAsTorus x y array2d =
      let nx = x |> modBy (Array2D.width array2d) in
      let ny = y |> modBy (Array2D.height array2d) in
      Array2D.get nx ny array2d

  in
    field
      |> Array2D.indexedMap (\x -> \y -> \l ->
        let
          lives =
            neighbors x y
              |> List.map (\(nx, ny) ->
                    field
                      |> getAsTorus nx ny
                      |> Maybe.withDefault Dead
              )
              |> List.filter (\nl -> nl /= Dead)
              |> List.length

          nextAlive =
            case l of
              Dead -> Alive 0
              Alive 7 -> Alive 7
              Alive n -> Alive (n+1)
        in
          case lives of
            0 -> Dead
            1 -> Dead
            2 ->
              if l /= Dead
              then nextAlive
              else Dead
            3 -> nextAlive
            o -> Dead
      )

addLife x y model =
  case Array2D.get x y model.field of
    Just Dead ->
      { model | field = model.field |> Array2D.set x y (Alive 0) }
    _ -> model

removeLife x y model =
  { model | field = model.field |> Array2D.set x y Dead }



-- VIEW

view {field, auto} =
  let w = (Array2D.width  field) * 20 |> String.fromInt in
  let h = (Array2D.height field) * 20 |> String.fromInt in
  div []
    [ div [ Mouse.onUp (\_ -> MouseUp) , Touch.onEnd (\_ -> MouseUp)]
      [ svg
        [ width w, height h, viewBox ("0 0 "++w++" "++h), class "life"]
        (drawLives field)
      ]
    , label []
      [ input [type_ "checkbox", onClick ToggleAuto][]
      , span [][text "auto"]
      ]
    , input [type_ "button", value "step", onClick Step][]
    , input [type_ "button", value "reset", onClick Reset][]
    ]

drawLives field =
  field
    |> Array2D.toIndexedList
    |> List.map (\(x, y, life) -> svgCircle x y life)

svgCircle x y life =
  circle
    [ cx ((x * 20) + 10 |> String.fromInt)
    , cy ((y * 20) + 10 |> String.fromInt)
    , r "8"
    , stroke "#888888"
    , fill (case life of
      Alive n -> svgColor n
      Dead -> "#FFFFFF")
    , Mouse.onDown (\_ -> MouseDown x y life)
    , Mouse.onUp (\_  -> MouseUp)
    , Mouse.onOver (\_ -> MouseOver x y)
    , Touch.onStart (\_ -> MouseDown x y life)
    , Touch.onEnd (\_  -> MouseUp)
    , Touch.onMove (\_ -> MouseOver x y)
    ][]

svgColor n =
  case n of
    1 -> "#FF8800"
    2 -> "#FFFF00"
    3 -> "#00FF00"
    4 -> "#00AA00"
    5 -> "#00FFFF"
    6 -> "#0000FF"
    7 -> "#FF00FF"
    _ -> "#880000"



-- SUBSCRIPTIONS

subscriptions {field, auto} =
  case auto of
    True -> Time.every 300 (\_ -> Step)
    False -> Sub.none