module Main exposing (..)

import Array exposing (Array)
import Browser
import Debug exposing (toString)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (repeat)
import Process exposing (sleep)
import Random exposing (generate)
import Random.Array exposing (shuffle)
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
    | FlipCardsFaceDown


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
    Array Card


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


initialDeck : Deck
initialDeck =
    List.concatMap (repeat 2 << FaceDown) [ A, B, C, D, E, F, G, H ]
        |> Array.fromList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle ->
            ( model, generate ShuffledDeck (shuffle initialDeck) )

        ShuffledDeck d ->
            ( { model | deck = d }, Cmd.none )

        CardFlipAttempt pos ->
            case modify pos flipCardFaceUp model.deck of
                Just newDeck ->
                    case model.selection of
                        NoneFlipped ->
                            ( { deck = newDeck, selection = OneFlipped pos }
                            , Cmd.none
                            )

                        OneFlipped p ->
                            if Array.get p newDeck == Array.get pos newDeck then
                                ( { deck = newDeck, selection = NoneFlipped }
                                , Cmd.none
                                )

                            else
                                ( { deck = newDeck
                                  , selection = BothFlipped p pos
                                  }
                                , Task.perform
                                    (always FlipCardsFaceDown)
                                    (sleep 2000.0)
                                )

                        BothFlipped _ _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        FlipCardsFaceDown ->
            case model.selection of
                NoneFlipped ->
                    ( model, Cmd.none )

                OneFlipped p ->
                    case modify p flipCardFaceDown model.deck of
                        Just newDeck ->
                            ( { deck = newDeck
                              , selection = NoneFlipped
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                BothFlipped p1 p2 ->
                    case
                        modify p1 flipCardFaceDown model.deck
                            |> Maybe.andThen (modify p2 flipCardFaceDown)
                    of
                        Just newDeck ->
                            ( { deck = newDeck
                              , selection = NoneFlipped
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )


flipCardFaceDown : Card -> Maybe Card
flipCardFaceDown card =
    case card of
        FaceDown _ ->
            Nothing

        FaceUp t ->
            Just (FaceDown t)


flipCardFaceUp : Card -> Maybe Card
flipCardFaceUp card =
    case card of
        FaceDown t ->
            Just (FaceUp t)

        FaceUp _ ->
            Nothing


modify : Int -> (a -> Maybe a) -> Array a -> Maybe (Array a)
modify idx fn xs =
    Array.get idx xs
        |> Maybe.andThen fn
        |> Maybe.map (\v -> Array.set idx v xs)



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
        (List.indexedMap viewCard (Array.toList deck))
