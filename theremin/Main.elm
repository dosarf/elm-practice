port module Main exposing (..)

import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html exposing (Html, button, div, h1, section, text, node)
import Window
import Task
import Mouse
import Element exposing (Element)
import Collage exposing (collage, path, traced, solid, move)
import Color exposing (..)


-- ports


port audio : Model -> Cmd msg


port visualization : (List Int -> msg) -> Sub msg



-- messages


type Msg
    = IncrementGain
    | DecrementGain
    | IncrementFrequency
    | DecrementFrequency
    | UpdateDimensions { width : Int, height : Int }
    | UpdateMouse { x : Int, y : Int }
    | Visualization (List Int)
    | NoOp



-- model


type alias Model =
    { gain : Float
    , frequency : Float
    , windowWidth : Int
    , windowHeight : Int
    , visualizationData : List Int
    }


init : ( Model, Cmd Msg )
init =
    ( { gain = 0.001
      , frequency = 3000
      , windowWidth = 100
      , windowHeight = 100
      , visualizationData = []
      }
    , getInitialWindowSize
    )



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        IncrementGain ->
            let
                newModel =
                    { model | gain = model.gain + 0.001 }
            in
                ( newModel, audio newModel )

        DecrementGain ->
            let
                newModel =
                    { model | gain = model.gain - 0.001 }
            in
                ( newModel, audio newModel )

        IncrementFrequency ->
            let
                newModel =
                    { model | frequency = model.frequency + 100 }
            in
                ( newModel, audio newModel )

        DecrementFrequency ->
            let
                newModel =
                    { model | frequency = model.frequency - 100 }
            in
                ( newModel, audio newModel )

        UpdateDimensions { width, height } ->
            let
                newModel =
                    { model
                        | windowWidth = width
                        , windowHeight = height
                    }
            in
                ( newModel, Cmd.none )

        UpdateMouse { x, y } ->
            let
                -- gain is the percentage you are across the screen, from left to right, mapped from 0 to 0.03
                newGain =
                    ((toFloat x) / (toFloat model.windowWidth)) * 0.03

                -- frequency is the percentage you are vertically down the screen, mapped from 0 to 6000
                newFrequency =
                    ((toFloat y) / (toFloat model.windowHeight)) * 6000.0

                newModel =
                    { model | frequency = newFrequency, gain = newGain }
            in
                ( newModel, audio newModel )

        Visualization intList ->
            ( { model | visualizationData = intList }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- view


view : Model -> Html Msg
view model =
    div []
        [ section []
            [ h1 [] [ text "Gain" ]
            , button [ onClick IncrementGain ] [ text "+" ]
            , text (toString model.gain)
            , button [ onClick DecrementGain ] [ text "-" ]
            ]
        , section []
            [ h1 [] [ text "Frequency" ]
            , button [ onClick IncrementFrequency ] [ text "+" ]
            , text (toString model.frequency)
            , button [ onClick DecrementFrequency ] [ text "-" ]
            ]
        , section []
            [ h1 [] [ text "Window dimensions" ]
            , text ("width: " ++ (toString model.windowWidth))
            , text ("height: " ++ (toString model.windowHeight))
            ]
        , div [ class "visualization" ]
            [ (visualizationGraph model) |> Element.toHtml ]
        ]



-- subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes UpdateDimensions
        , Mouse.moves UpdateMouse
        , visualization Visualization
        ]



-- main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- stuff


getInitialWindowSize : Cmd Msg
getInitialWindowSize =
    Task.perform UpdateDimensions Window.size



-- Our graph is a collage


visualizationGraph : Model -> Element
visualizationGraph model =
    let
        -- We turn the model into a set of points for a path
        points =
            (toPoints model)
    in
        -- We want the collage to have the same width and height as the window
        collage
            model.windowWidth
            model.windowHeight
            -- And it consists of a path across our points, traced solid red, and
            -- moved to the far left and vertical middle of the collage
            [ path points
                |> traced (solid red)
                |> move ( (toFloat model.windowWidth) / -2, (toFloat model.windowHeight) / -2 )
            ]



-- We need our whole model to create our list of points because it depends on
-- the window width and height.


toPoints : Model -> List ( Float, Float )
toPoints model =
    let
        -- The width of each slice is the window width divided by the number of
        -- data points we have.
        sliceWidth =
            (toFloat model.windowWidth) / (toFloat (List.length model.visualizationData))

        -- Turning a given piece of data into a point requires knowing its index in
        -- the list.
        indexedDatumToPoint n datum =
            let
                -- Its y coordinate should be its percentage (out of 128 total) times
                -- the window height, divided by 2.
                v =
                    (toFloat datum) / 128

                y =
                    (v * (toFloat model.windowHeight)) / 2

                -- And its x coordinate is its percentage of the total data set times
                -- the slice width.
                x =
                    sliceWidth * (toFloat n)
            in
                ( x, y )
    in
        model.visualizationData
            |> List.indexedMap indexedDatumToPoint
