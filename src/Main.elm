module Main exposing (..)

import Array exposing (Array)
import Browser
import Debug exposing (toString)
import Html exposing (Html, button, div, i, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import List exposing (repeat)
import Process exposing (sleep)
import Random exposing (generate)
import Random.Array exposing (shuffle)
import Task


main : Program Env Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { deck : Deck
    , selection : Selection
    , steps : Int
    , env : Env
    }


type alias Env =
    { isDev : Bool }


init : Env -> ( Model, Cmd Msg )
init env =
    ( { env = env, deck = initialDeck, selection = NoneFlipped, steps = 0 }
    , Task.perform identity (Task.succeed Shuffle)
      -- , Task.perform (\_ -> Shuffle) (Task.succeed ())
      -- , Task.perform (always Shuffle) (Task.succeed ())
    )


type Msg
    = NewGame
    | Shuffle
    | ShuffledDeck Deck
    | CardFlipAttempt Position
    | FlipCardsFaceDown
    | DEBUG_FlipCardsFaceUp


type CardType
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H


type CardFace
    = FaceUp
    | FaceDown


type alias Card =
    { face : CardFace, cardType : CardType }


type alias Deck =
    Array Card


type alias Position =
    Int


type Selection
    = NoneFlipped
    | OneFlipped Position
    | BothFlipped Position Position


initialDeck : Deck
initialDeck =
    List.concatMap (\t -> repeat 2 { face = FaceDown, cardType = t })
        [ A, B, C, D, E, F, G, H ]
        |> Array.fromList


debugFlipUp : Card -> Card
debugFlipUp c =
    { c | face = FaceUp }


flipDown : Card -> Card
flipDown c =
    { c | face = FaceDown }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DEBUG_FlipCardsFaceUp ->
            ( { model | deck = Array.map debugFlipUp model.deck }, Cmd.none )

        NewGame ->
            ( { model
                | deck = Array.map flipDown model.deck
                , selection = NoneFlipped
                , steps = 0
              }
            , Task.perform (always Shuffle) (sleep 750.0)
            )

        Shuffle ->
            ( model, generate ShuffledDeck (shuffle initialDeck) )

        ShuffledDeck d ->
            ( { model | deck = d }, Cmd.none )

        CardFlipAttempt pos ->
            if
                (Array.get pos model.deck |> Maybe.map .face)
                    == Just FaceDown
            then
                let
                    newDeck =
                        modify pos FaceUp model.deck
                in
                case model.selection of
                    NoneFlipped ->
                        ( { model
                            | deck = newDeck
                            , selection = OneFlipped pos
                            , steps = model.steps + 1
                          }
                        , Cmd.none
                        )

                    OneFlipped p ->
                        if Array.get p newDeck == Array.get pos newDeck then
                            ( { model
                                | deck = newDeck
                                , selection = NoneFlipped
                                , steps = model.steps + 1
                              }
                            , Cmd.none
                            )

                        else
                            ( { model
                                | deck = newDeck
                                , selection = BothFlipped p pos
                                , steps = model.steps + 1
                              }
                            , Task.perform
                                (always FlipCardsFaceDown)
                                (sleep 2000.0)
                            )

                    BothFlipped _ _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        FlipCardsFaceDown ->
            case model.selection of
                NoneFlipped ->
                    ( model, Cmd.none )

                OneFlipped p ->
                    ( { model
                        | deck = modify p FaceDown model.deck
                        , selection = NoneFlipped
                      }
                    , Cmd.none
                    )

                BothFlipped p1 p2 ->
                    ( { model
                        | deck =
                            modify p1 FaceDown model.deck
                                |> modify p2 FaceDown
                        , selection = NoneFlipped
                      }
                    , Cmd.none
                    )


modify : Position -> CardFace -> Deck -> Deck
modify pos face deck =
    Array.get pos deck
        |> Maybe.map (\card -> Array.set pos { card | face = face } deck)
        |> Maybe.withDefault deck



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


htmlIf : Html msg -> Bool -> Html msg
htmlIf el cond =
    if cond then
        el

    else
        text ""


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Steps:" ++ (model.steps |> toString)) ]
        , htmlIf (button [ onClick DEBUG_FlipCardsFaceUp ] [ text "Win it!" ])
            model.env.isDev
        , htmlIf viewVictory (allFaceUp model.deck)
        , div [ class "flex justify-center my-12" ] [ viewDeck model.deck ]
        ]


allFaceUp : Deck -> Bool
allFaceUp d =
    List.all (\c -> c.face == FaceUp) (Array.toList d)


viewVictory : Html Msg
viewVictory =
    div [ class "fixed z-10 inset-0 overflow-y-auto" ]
        [ div [ class "flex mt-28 items-start justify-center min-h-screen" ]
            [ div [ class "fixed inset-0 bg-black opacity-30" ] []
            , div [ class "relative bg-white rounded max-w-sm mx-auto" ]
                [ div
                    [ class "p-8 font-bold text-2xl text-center space-y-8" ]
                    [ div [] [ text "Congrats, you won!" ]
                    , button
                        [ onClick NewGame
                        , class "inline-flex items-center px-6 py-3 border border-transparent text-base font-medium rounded-md shadow-sm text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                        ]
                        [ text "New game" ]
                    ]
                ]
            ]
        ]


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
            case card.face of
                FaceUp ->
                    setInnerCardContainerAttrs pos
                        [ style "transform" "rotateY(180deg)" ]

                FaceDown ->
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
        [ card.cardType |> toString |> text ]


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
