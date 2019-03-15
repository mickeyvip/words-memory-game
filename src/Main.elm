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


type alias StateStarted =
    { currentSentence : String }


type alias StatePlaying =
    { sentence : Words
    }


type alias StateWin =
    { currentSentence : String
    }


type GameState
    = Started StateStarted
    | Playing StatePlaying
    | Win StateWin


type alias Model =
    GameState


initialModel : Model
initialModel =
    Started (StateStarted "The pen is mightier than the sword")


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


sentenceToString : Words -> String
sentenceToString sentence =
    sentence
        |> List.map wordToString
        |> String.join " "


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


generateSentence : String -> List Word
generateSentence sentenceString =
    sentenceString
        |> String.words
        |> List.map SentenceWrd


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( model, cmd ) =
            case initialModel of
                Started { currentSentence } ->
                    let
                        sentence : Words
                        sentence =
                            generateSentence currentSentence
                    in
                    ( Playing (StatePlaying sentence), chooseWords sentence )

                Playing _ ->
                    ( initialModel, Cmd.none )

                Win _ ->
                    ( initialModel, Cmd.none )
    in
    ( model, cmd )


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
                model_ =
                    case model of
                        Playing { sentence } ->
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
                                    List.indexedMap updateWord sentence

                                isWin_ =
                                    isWin (hiddenWords newSentence)
                            in
                            if isWin_ then
                                Win (StateWin (sentenceToString newSentence))

                            else
                                Playing (StatePlaying newSentence)

                        Started _ ->
                            model

                        Win _ ->
                            model
            in
            ( model_, Cmd.none )

        WordsChosen sortIndexes ->
            let
                model_ =
                    case model of
                        Playing { sentence } ->
                            let
                                sentence_ =
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
                                        sentence
                            in
                            Playing (StatePlaying sentence_)

                        Started _ ->
                            model

                        Win _ ->
                            model
            in
            ( model_, Cmd.none )

        NewGame ->
            init ()


view : Model -> Html Msg
view model =
    let
        currentView =
            case model of
                Playing { sentence } ->
                    viewGame sentence

                Started { currentSentence } ->
                    viewStarted currentSentence

                Win { currentSentence } ->
                    viewWin currentSentence
    in
    main_ [ class "section" ]
        [ div [ class "container" ]
            [ viewTitle
            , div [ class "box" ]
                [ currentView ]
            ]
        ]


viewStarted : String -> Html msg
viewStarted currentSentence =
    p []
        [ text currentSentence ]


viewGame : Words -> Html Msg
viewGame sentence =
    div []
        [ viewSentence sentence
        , viewHiddenWords (hiddenWords sentence)
        ]


viewWin : String -> Html Msg
viewWin sentence =
    div []
        [ div [ class "notification is-primary" ]
            [ h3 [ class "title is-3 has-text-centered" ] [ text "You Win!" ]
            ]
        , h4
            [ class "title is-4 has-text-centered" ]
            [ p
                [ class "has-text-centered" ]
                [ text sentence ]
            ]
        , div [ class "is-clearfix" ]
            [ button
                [ class "button is-info is-pulled-right"
                , onClick NewGame
                ]
                [ text "New Game" ]
            ]
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
