module Route exposing (..)


type Location
    = Home
    | Topics


type alias Model =
    Maybe Location


init : Maybe Location -> Model
init location =
    location
