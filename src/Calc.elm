module Calc exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E


type alias Model =
  { number1 : Int
  , number2 : Int
  , result : Int
  }

type Msg
  = OnNumber1 String
  | OnNumber2 String
  | Add
  | Sub

init : Model
init =
  { number1 = 0
  , number2 = 0
  , result = 0
  }

type Field = First | Second

parseTheInt : Field -> String -> Model -> Model
parseTheInt field value model =
  case String.toInt value of
    Nothing  -> model
    Just val ->
      case field of
        First  -> { model | number1 = val }
        Second -> { model | number2 = val }

update : Msg -> Model -> Model
update msg model =
  case msg of
    OnNumber1 value -> parseTheInt First value model
    OnNumber2 value -> parseTheInt Second value model
    Add -> { model | result = model.number1 + model.number2 }
    Sub -> { model | result = model.number1 - model.number2 }

viewInput : (String -> Msg) -> Int -> Html Msg
viewInput msg value =
  Html.input
    [ A.type_ "number"
    , A.placeholder "10"
    , A.value (String.fromInt value)
    , E.onInput msg
    ]
    []

viewButton : Msg -> String -> Html Msg
viewButton msg title =
  Html.button
    [ E.onClick msg ]
    [ Html.text title ]

view : Model -> Html Msg
view model =
  Html.div []
    [ Html.h1 []
      [ Html.text "Calc" ]
    , viewInput OnNumber1 model.number1
    , viewInput OnNumber2 model.number2
    , viewButton Add "Add"
    , viewButton Sub "Sub"
    , Html.div []
      [ Html.h2 [] [ Html.text "Result:" ]
      , Html.text (String.fromInt model.result)
      ]
    ]

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }
