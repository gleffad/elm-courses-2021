module Calc exposing (..)

import Browser
import Html exposing (Html)

type alias Model =
  {}

type Msg
  = NoOp

init : Model
init = {}

update : Msg -> Model -> Model
update msg model = model

view : Model -> Html Msg
view model = Html.text ""

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }
