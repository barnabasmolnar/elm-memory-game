module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List exposing (repeat)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    Int


init : Model
init =
    0


type Msg
    = Increment
    | Decrement


type CardType
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H


type Card
    = FaceUp CardType
    | FaceDown CardType


type Deck
    = List Card


type alias Position =
    Int


type Selection
    = NoneFlipped
    | OneFlipped Position
    | BothFlipped Position Position


type alias GameState =
    { deck : Deck
    , selection : Selection
    }


initialDeck : List Card
initialDeck =
    List.concatMap (repeat 2 << FaceDown) [ A, B, C, D, E, F, G, H ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
