module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (repeat)
import Random exposing (generate)
import Random.List exposing (shuffle)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    GameState


init : () -> ( Model, Cmd Msg )
init _ =
    ( { deck = initialDeck, selection = NoneFlipped }
    , Task.perform identity (Task.succeed Shuffle)
      -- , Task.perform (\_ -> Shuffle) (Task.succeed ())
      -- , Task.perform (always Shuffle) (Task.succeed ())
    )


type Msg
    = Shuffle
    | ShuffledDeck Deck
    | CardFlipAttempt Position


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


type alias Deck =
    List Card


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle ->
            ( model, generate ShuffledDeck (shuffle initialDeck) )

        ShuffledDeck d ->
            ( { model | deck = d }, Cmd.none )

        CardFlipAttempt pos ->
            ( { model
                | deck =
                    List.indexedMap
                        (\idx card ->
                            if idx == pos then
                                flipCard card

                            else
                                card
                        )
                        model.deck
              }
            , Cmd.none
            )


flipCard : Card -> Card
flipCard card =
    case card of
        FaceDown t ->
            FaceUp t

        FaceUp t ->
            FaceDown t



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [] [ viewDeck model.deck ]


viewCard : Position -> Card -> Html Msg
viewCard pos card =
    div
        [ style "border" "2px solid black"
        , style "width" "40px"
        , style "height" "60px"
        , style "background-color" "grey"
        , onClick (CardFlipAttempt pos)
        ]
        [ case card of
            FaceDown _ ->
                text ""

            FaceUp t ->
                text (toString t)
        ]


viewDeck : Deck -> Html Msg
viewDeck deck =
    div
        [ style "display" "grid"
        , style "width" "max-content"
        , style "grid-template-columns" "repeat(4, 1fr)"
        , style "gap" "10px"
        ]
        (List.indexedMap viewCard deck)
