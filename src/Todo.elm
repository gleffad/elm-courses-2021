module Todo exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E

type alias Todo =
  { title : String
  , content : String
  }

type alias Todos =
  { inProgress : List Todo
  , over : List Todo
  , future : List Todo
  }

type alias Model =
  { todos : Todos
  , newTodoForm :
    { title : String
    , content : String
    }
  }

type Msg
  = UpdateTitle String
  | UpdateContent String
  | CreateTodo

init : Model
init =
  { todos =
    { inProgress =
      [ Todo "First" "First body"
      , Todo "It's me" "Mario"
      ]
    , over = [ Todo "Second" "Second body" ]
    , future = [ Todo "Third" "Third body" ]
    }
  , newTodoForm =
    { title = ""
    , content = ""
    }
  }

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateTitle value ->
      let f = model.newTodoForm
          todoForm = { f | title = value } in
      { model | newTodoForm = todoForm }
      
    UpdateContent value ->
      let f = model.newTodoForm
          todoForm = { f | content = value } in
      { model | newTodoForm = todoForm }

    CreateTodo ->
      let
        newTodo : Todo
        newTodo =
          { title = model.newTodoForm.title
          , content = model.newTodoForm.content
          }

        f : Todos
        f = model.todos

        newTodos : Todos
        newTodos = { f | inProgress = newTodo :: f.inProgress }

        form : { title : String, content : String }
        form = { title = "", content = "" }
      in
        { model
          | todos = newTodos
          , newTodoForm = form
        }




viewTodo : Todo -> Html Msg
viewTodo todo =
  H.div
    [ A.style "border" "1px solid #aaa"
    , A.style "margin" "12px 0"
    , A.style "padding" "12px"
    , A.style "border-radius" "10px"
    ]
    [ H.text todo.title
    , H.br [] []
    , H.text todo.content
    ]

view : Model -> Html Msg
view model =
  H.div
    [ A.style "margin" "12px" ]
    [ H.div
      [ A.style "display" "flex"
      , A.style "flex-direction" "column"
      ]
      [ H.input
        [ E.onInput UpdateTitle
        , A.value model.newTodoForm.title
        ]
        []
      , H.input
        [ E.onInput UpdateContent
        , A.value model.newTodoForm.content
        ]
        []
      , H.input
        [ A.type_ "submit"
        , E.onClick CreateTodo
        ]
        []
      ]
    , H.h1 [] [ H.text "In progress" ]
    , H.div []
      (List.map viewTodo model.todos.inProgress)
    , H.h1 [] [ H.text "Over" ]
    , H.div []
      (List.map viewTodo model.todos.over)
    , H.h1 [] [ H.text "Future" ]
    , H.div []
      (List.map viewTodo model.todos.future)
    ]

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }
