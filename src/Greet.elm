module Greet exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events

type alias Model =
  { name : String
  , hello : String
  }

type Msg
  = UpdateInput String
  | SayHello

init : Model
init = { name = "", hello = "" }

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateInput value -> { model | name = value }
    SayHello ->
      { model
        | hello = model.name
        , name = ""
      }

view : Model -> Html Msg
view model =
  Html.div []
    [ Html.input
      [ Html.Events.onInput UpdateInput
      , Html.Attributes.value model.name
      ]
      []
    , Html.button
      [ Html.Events.onClick SayHello ]
      [ Html.text "Dis bonjour" ]
    , Html.text
      (if String.length model.hello == 0 then
        ""
       else
         "Bonjour " ++ model.hello
      )
    ]

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }
