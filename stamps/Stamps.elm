module Stamps exposing (..)

import Color
import Collage
import Element
import Html exposing (Html)
import Mouse
import Keyboard


type alias Position =
    { x : Int
    , y : Int
    }


type Shape
    = Pentagon
    | Circle


type alias Stamp =
    { position : Position
    , shape : Shape
    }


type alias Model =
    { stamps : List Stamp
    , shift : Bool
    }


initialModel : Model
initialModel =
    { stamps = [ (Stamp (Position 200 -100) Pentagon) ]
    , shift = False
    }


type Msg
    = AddClick Position
    | HandleShift Bool
    | ClearAll
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        AddClick position ->
            let
                newShape =
                    if model.shift then
                        Pentagon
                    else
                        Circle

                newStamp =
                    Stamp position newShape
            in
                { model | stamps = List.append model.stamps [ newStamp ] } ! []

        HandleShift shiftPressed ->
            { model | shift = shiftPressed } ! []

        NoOp ->
            model ! []

        ClearAll ->
            { model | stamps = [] } ! []


view : Model -> Html Msg
view model =
    let
        theGroup =
            -- Map a list of positions through our drawstamp function to get a list
            -- of forms, and put them in a group
            Collage.group (List.map drawStamp model.stamps)

        -- We'll move the group to the origin like we did in the previous examples
        originGroup =
            Collage.move ( -400, 400 ) theGroup
    in
        -- Now make a collage containing the group
        Collage.collage
            800
            800
            [ originGroup ]
            |> Element.toHtml


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks (\{ x, y } -> AddClick (Position x (-1 * y)))
        , Keyboard.downs mapKeyDown
        , Keyboard.ups mapKeyUp
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- graphics


drawStamp : Stamp -> Collage.Form
drawStamp stamp =
    let
        -- We'll extract x and y
        position =
            stamp.position

        -- and we'll make a different shape based on the stamp's shape
        shape =
            case stamp.shape of
                Pentagon ->
                    Collage.ngon 5 50

                Circle ->
                    Collage.circle 50

        color =
            case stamp.shape of
                Pentagon ->
                    Color.yellow

                Circle ->
                    Color.red
    in
        shape
            |> Collage.filled color
            |> Collage.move ( toFloat (position.x), toFloat (position.y) )



-- event handling


mapKeyDown : Int -> Msg
mapKeyDown keyCode =
    case keyCode of
        16 ->
            HandleShift True

        _ ->
            NoOp


mapKeyUp : Int -> Msg
mapKeyUp keyCode =
    case keyCode of
        16 ->
            HandleShift False

        67 ->
            ClearAll

        _ ->
            NoOp
