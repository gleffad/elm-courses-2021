module Simple exposing (..)

import Browser
import Html exposing (Html)
import Html.Events

type alias Model =
  { count : Int }

type Msg
  = Increment Int
  | Decrement Int
  | Reset

init : Model
init = { count = 0 }

increment : Int -> Model -> Model
increment value model =
  { model | count = model.count + value }

decrement : Int -> Model -> Model
decrement value model =
  { model | count = model.count - value }

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment value -> increment value model
    Decrement value -> decrement value model
    Reset -> { model | count = 0 }

viewButton : Msg -> String -> Html Msg
viewButton msg content =
  Html.button
    [ Html.Events.onClick msg ]
    [ Html.text content ]

view : Model -> Html Msg
view model =
  Html.div []
    [ viewButton (Increment 1) "+1"
    , viewButton (Increment 2) "+2"
    , Html.text (String.fromInt model.count)
    , viewButton (Decrement 1) "-1"
    , viewButton (Decrement 2) "-2"
    , viewButton Reset "Reset"
    ]

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }
