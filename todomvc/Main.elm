port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (autofocus, checked, class, classList, href, placeholder, type_, value)
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput)
import Json.Decode
import Json.Encode
import Json.Decode.Pipeline


type alias Todo =
    { title : String
    , completed : Bool
    , editing : Bool
    , identifier : Int
    }



-- We have the filter state for the application


type FilterState
    = All
    | Active
    | Completed



-- We have the entire application state's model


type alias Model =
    { todos : List Todo
    , todo : Todo
    , filter : FilterState
    , nextIdentifier : Int
    }



-- We have the messages that can occur


type Msg
    = Add
    | ToggleComplete Int
    | Delete Todo
    | UpdateField String
    | Filter FilterState
    | ClearCompleted
    | SetModel Model
    | NoOp


newTodo =
    Todo "" False False 0


initialModel =
    { todos =
        [ { title = "The first todo"
          , completed = False
          , editing = False
          , identifier = 1
          }
        ]
    , todo = { newTodo | identifier = 2 }
    , filter = All
    , nextIdentifier = 3
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            let
                newModel =
                    { model
                        | todos = model.todo :: model.todos
                        , todo = { newTodo | identifier = model.nextIdentifier }
                        , nextIdentifier = model.nextIdentifier + 1
                    }
            in
                ( newModel
                , sendToStorage newModel
                )

        ToggleComplete identifier ->
            let
                toggleTodo : Todo -> Todo
                toggleTodo someTodo =
                    if someTodo.identifier == identifier then
                        { someTodo | completed = not someTodo.completed }
                    else
                        someTodo

                newModel =
                    { model | todos = model.todos |> List.map toggleTodo }
            in
                ( newModel
                , sendToStorage newModel
                )

        Delete todo ->
            let
                validTodoFilter =
                    (\t -> t.identifier /= todo.identifier)

                newModel =
                    { model | todos = model.todos |> List.filter validTodoFilter }
            in
                ( newModel
                , sendToStorage newModel
                )

        UpdateField str ->
            let
                oldTodo =
                    model.todo

                newModel =
                    { model | todo = { oldTodo | title = str } }
            in
                ( newModel
                , sendToStorage newModel
                )

        Filter filterState ->
            let
                newModel =
                    { model | filter = filterState }
            in
                ( newModel
                , sendToStorage newModel
                )

        ClearCompleted ->
            let
                newModel =
                    { model | todos = model.todos |> List.filter (\t -> not t.completed) }
            in
                ( newModel
                , sendToStorage newModel
                )

        SetModel storedModel ->
            ( storedModel
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ section [ class "todoapp" ]
            [ header [ class "header" ]
                [ h1 [] [ text "todos" ]
                , input
                    [ class "new-todo"
                    , placeholder "What needs to be done?"
                    , value model.todo.title
                    , autofocus True
                    , onEnter Add
                    , onInput UpdateField
                    ]
                    []
                ]
            , section [ class "main" ]
                [ ul [ class "todo-list" ]
                    (model |> filterTodos |> List.map todoView)
                ]
            , footer [ class "footer" ]
                [ span [ class "todo-count" ]
                    [ strong [] [ text (activeTodoCount model |> toString) ]
                    , text " items left"
                    ]
                , ul [ class "filters" ]
                    [ filterItemView model All
                    , filterItemView model Active
                    , filterItemView model Completed
                    ]
                , button
                    [ class "clear-completed"
                    , onClick ClearCompleted
                    ]
                    [ text "Clear completed" ]
                ]
            ]
        ]


todoView : Todo -> Html Msg
todoView todo =
    li [ classList [ ( "completed", todo.completed ) ] ]
        [ div [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onCheck (\_ -> ToggleComplete todo.identifier)
                ]
                []
            , label [] [ text todo.title ]
            , button
                [ class "destroy"
                , onClick (Delete todo)
                ]
                []
            ]
        ]


filterTodos : Model -> List Todo
filterTodos model =
    let
        filterFunction =
            case model.filter of
                All ->
                    (\todo -> True)

                Active ->
                    (\todo -> not todo.completed)

                Completed ->
                    (\todo -> todo.completed)
    in
        model.todos |> List.filter filterFunction


filterItemView : Model -> FilterState -> Html Msg
filterItemView model filterState =
    li []
        [ a
            [ classList [ ( "selected", (model.filter == filterState) ) ]
            , href "#"
            , onClick (Filter filterState)
            ]
            [ text (toString filterState) ]
        ]


activeTodoCount : Model -> Int
activeTodoCount model =
    model.todos
        |> List.filter (\todo -> not todo.completed)
        |> List.length


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed msg
            else
                Json.Decode.fail "not the right keycode"
    in
        on "keydown" (keyCode |> Json.Decode.andThen isEnter)


subscriptions : Model -> Sub Msg
subscriptions model =
    storageInput mapStorageInput


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port storage : Json.Encode.Value -> Cmd msg



-- We'll define how to encode our Model to Json.Encode.Values


encodeModel : Model -> Json.Encode.Value
encodeModel model =
    -- It's a json object with a list of fields
    Json.Encode.object
        -- The `todos` field is a list of encoded Todos, we'll define this encodeTodo function later
        [ ( "todos", Json.Encode.list (List.map encodeTodo model.todos) )
          -- The current todo is also going to go through encodeTodo
        , ( "todo", encodeTodo model.todo )
          -- The filter gets encoded with a custom function as well
        , ( "filter", encodeFilterState model.filter )
          -- And the next identifier is just an int
        , ( "nextIdentifier", Json.Encode.int model.nextIdentifier )
        ]



-- We'll define how to encode a Todo


encodeTodo : Todo -> Json.Encode.Value
encodeTodo todo =
    -- It's an object with a list of fields
    Json.Encode.object
        -- The title is a string
        [ ( "title", Json.Encode.string todo.title )
          -- completed is a bool
        , ( "completed", Json.Encode.bool todo.completed )
          -- editing is a bool
        , ( "editing", Json.Encode.bool todo.editing )
          -- identifier is an int
        , ( "identifier", Json.Encode.int todo.identifier )
        ]



-- The FilterState encoder takes a FilterState and returns a Json.Encode.Value


encodeFilterState : FilterState -> Json.Encode.Value
encodeFilterState filterState =
    -- We'll use toString to turn our FilterState into a string
    Json.Encode.string (toString filterState)


sendToStorage : Model -> Cmd msg
sendToStorage model =
    encodeModel model |> storage


port storageInput : (Json.Decode.Value -> msg) -> Sub msg


mapStorageInput : Json.Decode.Value -> Msg
mapStorageInput modelJson =
    case (decodeModel modelJson) of
        Ok model ->
            SetModel model

        Err errorMessage ->
            let
                -- If there's an error decoding it, we can show it in the
                -- console
                _ =
                    Debug.log "Error in mapStorageInput:" errorMessage
            in
                NoOp



-- The actual Decode function takes the modelJson and returns a result.  Here we
-- invoke the `decodeValue` function, where the first argument is a Decoder and
-- the second is the JSON string we're decoding


decodeModel : Json.Decode.Value -> Result String Model
decodeModel modelJson =
    Json.Decode.decodeValue modelDecoder modelJson



-- This brings us to the Decoder.  This returns a Decoder for a Model.  It uses
-- the `map4` function, which just decodes an object with 4 keys in it.  We
-- tell it to produce a Model, and then we describe the mapping for each of the
-- fields.  The first argument is actually a function that should take 4
-- arguments - in this case, it's the `Model` constructor function that the
-- record alias type produces for us, but it could be any 4-arity function.
{-
   modelDecoder : Json.Decode.Decoder Model
   modelDecoder =
       Json.Decode.map4 Model
           -- For todos, we return a list passed through a new todoDecoder.
           -- We use `Decode.field` to specify each field we're decoding.
           (Json.Decode.field "todos" (Json.Decode.list todoDecoder))
           -- The active todo also goes through the todoDecoder
           (Json.Decode.field "todo" todoDecoder)
           -- The filter gets decoded as a string, then mapped to a FilterState
           (Json.Decode.field "filter" (Json.Decode.string |> Json.Decode.map filterStateDecoder))
           -- Then nextIdentifier just uses the builtin int decoder
           (Json.Decode.field "nextIdentifier" Json.Decode.int)
-}


modelDecoder : Json.Decode.Decoder Model
modelDecoder =
    Json.Decode.Pipeline.decode Model
        |> Json.Decode.Pipeline.required "todos" (Json.Decode.list todoDecoder)
        |> Json.Decode.Pipeline.required "todo" todoDecoder
        |> Json.Decode.Pipeline.required "filter" (Json.Decode.string |> Json.Decode.map filterStateDecoder)
        |> Json.Decode.Pipeline.required "nextIdentifier" Json.Decode.int



-- Our todoDecoder is another `map4` for a Todo data structure.
{-
   todoDecoder : Json.Decode.Decoder Todo
   todoDecoder =
       Json.Decode.map4 Todo
           -- Our title is a string
           (Json.Decode.field "title" Json.Decode.string)
           -- completed is a bool
           (Json.Decode.field "completed" Json.Decode.bool)
           -- editing is a bool
           (Json.Decode.field "editing" Json.Decode.bool)
           -- and identifier is an int
           (Json.Decode.field "identifier" Json.Decode.int)
-}


todoDecoder : Json.Decode.Decoder Todo
todoDecoder =
    Json.Decode.Pipeline.decode Todo
        |> Json.Decode.Pipeline.required "title" Json.Decode.string
        |> Json.Decode.Pipeline.required "completed" Json.Decode.bool
        |> Json.Decode.Pipeline.required "editing" Json.Decode.bool
        |> Json.Decode.Pipeline.required "identifier" Json.Decode.int



-- Now all that's left is the filterStateDecoder.  For this we'll just use
-- `Json.Decode.map` to map through a function that turns a String into a
-- FilterState.


filterStateDecoder : String -> FilterState
filterStateDecoder string =
    case string of
        "All" ->
            All

        "Active" ->
            Active

        "Completed" ->
            Completed

        _ ->
            let
                _ =
                    Debug.log "filterStateDecoder" <|
                        "Couldn't decode value "
                            ++ string
                            ++ " so defaulting to All."
            in
                All
