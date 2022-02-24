module Main exposing (..)

import Array exposing (Array)
import Browser
import Debug exposing (toString)
import Html exposing (Html, div, i, span, text)
import Html.Attributes exposing (class, style)
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
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div [ class "flex justify-center my-12" ] [ viewDeck model.deck ]


viewCard : Position -> Card -> Html Msg
viewCard pos card =
    div
        [ class "w-24 h-36 bg-transparent [perspective:1000px]" ]
        [ viewInnerCardContainer pos
            card
            [ viewCardFaceUp card
            , viewCardFaceDown
            ]
        ]


viewDeck : Deck -> Html Msg
viewDeck deck =
    div
        [ class "grid grid-cols-4 gap-3 w-max" ]
        (List.indexedMap viewCard (Array.toList deck))


returnCardType : Card -> CardType
returnCardType card =
    case card of
        FaceUp t ->
            t

        FaceDown t ->
            t


setInnerCardContainerAttrs : Position -> List (Html.Attribute Msg) -> List (Html.Attribute Msg)
setInnerCardContainerAttrs pos attrs =
    [ onClick (CardFlipAttempt pos)
    , class "relative"
    , class "w-full h-full"
    , class "transition-transform duration-500"
    , class "shadow-md"
    , class "[transform-style:preserve-3d]"
    ]
        ++ attrs


viewInnerCardContainer : Position -> Card -> List (Html Msg) -> Html Msg
viewInnerCardContainer pos card faces =
    let
        attrs =
            case card of
                FaceUp _ ->
                    setInnerCardContainerAttrs pos
                        [ style "transform" "rotateY(180deg)" ]

                FaceDown _ ->
                    setInnerCardContainerAttrs pos []
    in
    div attrs faces


viewCardFaceUp : Card -> Html msg
viewCardFaceUp card =
    div
        [ class "absolute"
        , class "w-full h-full"
        , class "bg-slate-100"
        , class "flex items-center justify-center"
        , class "text-4xl font-bold text-slate-600"
        , class "[backface-visibility:hidden]"
        , class "[transform:rotateY(180deg)]"
        ]
        [ returnCardType card |> toString |> text ]


viewCardFaceDown : Html msg
viewCardFaceDown =
    div
        [ class "absolute"
        , class "w-full h-full"
        , class "bg-slate-300"
        , class "flex items-center justify-center"
        , class "[backface-visibility:hidden]"
        ]
        [ span [ class "text-4xl font-bold text-slate-400 block" ]
            [ i [ class "fas fa-gem" ] [] ]
        ]
