module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, h4, i, li, main_, option, p, select, span, text, ul)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onClick, onInput)
import Random
import Random.List


type PlayerChoice
    = PlayerChoice String


type Answer
    = Answer String


type alias HiddenWord =
    { sortKey : Int
    , playerChoice : PlayerChoice
    , answer : Answer
    }


type Word
    = SentenceWrd String
    | HiddenWrd HiddenWord


type alias Words =
    List Word


type alias Model =
    { sentence : Words
    }


initialModel : Model
initialModel =
    { sentence =
        [ SentenceWrd "The"
        , SentenceWrd "pen"
        , SentenceWrd "is"
        , SentenceWrd "mightier"
        , SentenceWrd "than"
        , SentenceWrd "the"
        , SentenceWrd "sword"
        ]
    }


type Msg
    = WordChanged Int String
    | WordsChosen (List ( Int, Int ))
    | NewGame


playerChoiceToString : PlayerChoice -> String
playerChoiceToString (PlayerChoice stringValue) =
    stringValue


answerToString : Answer -> String
answerToString (Answer stringValue) =
    stringValue


wordToString : Word -> String
wordToString word =
    case word of
        SentenceWrd wordString ->
            wordString

        HiddenWrd { answer } ->
            answerToString answer


hiddenWords : Words -> List HiddenWord
hiddenWords sentence =
    List.filterMap
        (\word ->
            case word of
                HiddenWrd hiddenWord ->
                    Just hiddenWord

                _ ->
                    Nothing
        )
        sentence


hiddenWordsSorted : List HiddenWord -> List HiddenWord
hiddenWordsSorted hiddenWordList =
    List.sortBy .sortKey hiddenWordList


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, chooseWords initialModel.sentence )


isWin : List HiddenWord -> Bool
isWin =
    List.all
        (\{ answer, playerChoice } ->
            answerToString answer == playerChoiceToString playerChoice
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WordChanged index wordString ->
            let
                updateWord : Int -> Word -> Word
                updateWord wordIndex word =
                    case word of
                        HiddenWrd hiddenWord ->
                            if wordIndex == index then
                                HiddenWrd
                                    { hiddenWord
                                        | playerChoice = PlayerChoice wordString
                                    }

                            else
                                word

                        _ ->
                            word

                newSentence : Words
                newSentence =
                    List.indexedMap updateWord model.sentence
            in
            ( { model | sentence = newSentence }, Cmd.none )

        WordsChosen sortIndexes ->
            let
                sentence =
                    List.indexedMap
                        (\wordIndex word ->
                            let
                                hiddenIndex =
                                    List.filter
                                        (\( _, originalWordIndex ) ->
                                            originalWordIndex == wordIndex
                                        )
                                        sortIndexes

                                newWord =
                                    case List.head hiddenIndex of
                                        Just ( sortKey, _ ) ->
                                            HiddenWrd
                                                { sortKey = sortKey
                                                , answer = Answer (wordToString word)
                                                , playerChoice = PlayerChoice ""
                                                }

                                        Nothing ->
                                            word
                            in
                            newWord
                        )
                        model.sentence
            in
            ( { model | sentence = sentence }, Cmd.none )

        NewGame ->
            ( initialModel, chooseWords initialModel.sentence )


view : Model -> Html Msg
view model =
    let
        win : Bool
        win =
            isWin <| hiddenWords model.sentence

        currentView : Html Msg
        currentView =
            if win then
                viewWin model.sentence

            else
                viewGame model.sentence
    in
    main_ [ class "section" ]
        [ div [ class "container" ]
            [ viewTitle
            , div [ class "box" ]
                [ currentView ]
            ]
        ]


viewGame : Words -> Html Msg
viewGame sentence =
    div []
        [ viewSentence sentence
        , viewHiddenWords (hiddenWords sentence)
        ]


viewWin : Words -> Html Msg
viewWin sentence =
    div []
        [ div [ class "notification is-primary" ]
            [ h3 [ class "title is-3 has-text-centered" ] [ text "You Win!" ]
            ]
        , h4
            [ class "title is-4 has-text-centered" ]
            [ viewOriginalSentence sentence ]
        , div [ class "is-clearfix" ]
            [ button
                [ class "button is-info is-pulled-right"
                , onClick NewGame
                ]
                [ text "New Game" ]
            ]
        ]


viewOriginalSentence : Words -> Html Msg
viewOriginalSentence words =
    p
        [ class "has-text-centered" ]
        [ words
            |> List.map wordToString
            |> String.join " "
            |> text
        ]


viewHiddenWords : List HiddenWord -> Html msg
viewHiddenWords hiddenWordList =
    let
        hiddenWordsSorted_ : List HiddenWord
        hiddenWordsSorted_ =
            hiddenWordsSorted hiddenWordList

        viewHiddenWord_ : HiddenWord -> Html msg
        viewHiddenWord_ hiddenWord =
            let
                answerString : String
                answerString =
                    answerToString hiddenWord.answer

                playerChoiceString : String
                playerChoiceString =
                    playerChoiceToString hiddenWord.playerChoice

                isCorrectGuess : Bool
                isCorrectGuess =
                    answerString == playerChoiceString

                className : String
                className =
                    if isCorrectGuess then
                        "has-text-success"

                    else
                        "has-text-grey-light"
            in
            li []
                [ span [ class className ]
                    [ text answerString
                    , text " "
                    , span [ class "icon is-small" ]
                        [ i [ class "far fa-check-circle" ] [] ]
                    ]
                ]
    in
    ul [] <| List.map viewHiddenWord_ hiddenWordsSorted_


viewSentence : Words -> Html Msg
viewSentence sentence =
    let
        hiddenWords_ : List HiddenWord
        hiddenWords_ =
            hiddenWords sentence
    in
    div [ class "has-text-centered" ]
        (List.indexedMap
            (\index sentenceWord ->
                case sentenceWord of
                    SentenceWrd word ->
                        span [ class "sentence-word" ] [ text word ]

                    HiddenWrd hiddenWord ->
                        viewHiddenWord index hiddenWord hiddenWords_
            )
            sentence
        )


viewHiddenWord : Int -> HiddenWord -> List HiddenWord -> Html Msg
viewHiddenWord index hiddenWord hiddenWords_ =
    let
        hiddenWordsSorted_ : List HiddenWord
        hiddenWordsSorted_ =
            hiddenWordsSorted hiddenWords_

        playerChoiceString : String
        playerChoiceString =
            playerChoiceToString hiddenWord.playerChoice

        viewOption : HiddenWord -> Html Msg
        viewOption { answer } =
            let
                answerString : String
                answerString =
                    answerToString answer
            in
            option
                [ value answerString, selected (answerString == playerChoiceString) ]
                [ text <| String.toLower answerString ]
    in
    div [ class "select" ]
        [ select [ class "hidden-word", onInput (WordChanged index) ] <|
            option []
                [ text "" ]
                :: List.map viewOption hiddenWordsSorted_
        ]


viewTitle : Html msg
viewTitle =
    h1 [ class "title has-text-centered" ]
        [ text "Words Memory Game" ]



--- COMMANDS ----


chooseWords : Words -> Cmd Msg
chooseWords chosenSentence =
    List.range 0 (List.length chosenSentence - 1)
        |> Random.List.shuffle
        |> Random.map (List.take 3)
        |> Random.map (List.indexedMap Tuple.pair)
        |> Random.generate WordsChosen



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
