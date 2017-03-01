module Main exposing (..)

import Color
import Collage
import Element
import Html exposing (Html)
import Mouse


canvasWidth : Int
canvasWidth =
    800


canvasHeight : Int
canvasHeight =
    800


type alias Model =
    { position : Mouse.Position
    }


initialModel : Model
initialModel =
    { position =
        { x = 0
        , y = 0
        }
    }


type Msg
    = MouseMove Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MouseMove newPosition ->
            { model | position = newPosition } ! []


view : Model -> Html Msg
view model =
    shapes ( toFloat model.position.x, -(toFloat model.position.y) )
        |> Element.toHtml


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.moves MouseMove


main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


shapes : ( Float, Float ) -> Element.Element
shapes ( x, y ) =
    let
        group =
            [ Collage.move ( 0, -55 ) blueSquare
            , Collage.move ( 0, 55 ) redSquare
            , Collage.move ( -110, -55 ) blueCircle
            , Collage.move ( -110, 55 ) redCircle
            , Collage.move ( 110, -55 ) blueHexagon
            , Collage.move ( 110, 55 ) redPentagon
            ]

        originGroup =
            group |> List.map (Collage.move ( -canvasWidth // 2 |> toFloat, canvasHeight // 2 |> toFloat ))

        movedGroup =
            originGroup |> List.map (Collage.move ( x, y ))
    in
        Collage.collage
            canvasWidth
            canvasHeight
            movedGroup


blueSquare : Collage.Form
blueSquare =
    Collage.outlined (Collage.dashed Color.blue) square


redSquare : Collage.Form
redSquare =
    Collage.outlined (Collage.solid Color.red) square


square : Collage.Shape
square =
    Collage.square 100


blueCircle : Collage.Form
blueCircle =
    Collage.filled Color.blue circle


redCircle : Collage.Form
redCircle =
    Collage.filled Color.red circle


circle : Collage.Shape
circle =
    Collage.circle 50


blueHexagon : Collage.Form
blueHexagon =
    Collage.filled Color.blue (Collage.ngon 6 50)


redPentagon : Collage.Form
redPentagon =
    Collage.filled Color.red (Collage.ngon 5 50)
