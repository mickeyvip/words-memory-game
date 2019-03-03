module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, i, li, main_, option, p, select, span, text, ul)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)


type PlayerChoice
    = PlayerChoice String


type Answer
    = Answer String


type Word
    = SentenceWrd String
    | HiddenWrd HiddenWord


type alias HiddenWord =
    { sortKey : Int
    , playerChoice : PlayerChoice
    , answer : Answer
    }


type alias Words =
    List Word


type alias Model =
    { originalSentence : String
    , sentence : Words
    }


initialModel : Model
initialModel =
    { originalSentence = "The pen is mightier than the sword"
    , sentence =
        [ SentenceWrd "The"
        , HiddenWrd { sortKey = 3, answer = Answer "pen", playerChoice = PlayerChoice "" }
        , SentenceWrd "is"
        , HiddenWrd { sortKey = 1, answer = Answer "mightier", playerChoice = PlayerChoice "" }
        , SentenceWrd "than"
        , SentenceWrd "the"
        , HiddenWrd { sortKey = 2, answer = Answer "sword", playerChoice = PlayerChoice "" }
        ]
    }


type Msg
    = WordChanged Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        WordChanged index wordString ->
            let
                updateWord : Int -> Word -> Word
                updateWord wordIndex word =
                    case word of
                        HiddenWrd hiddenWord ->
                            if wordIndex == index then
                                HiddenWrd { hiddenWord | playerChoice = PlayerChoice wordString }

                            else
                                word

                        _ ->
                            word

                newSentence : Words
                newSentence =
                    List.indexedMap updateWord model.sentence
            in
            { model | sentence = newSentence }


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


answers : List HiddenWord -> List Answer
answers sentenceHiddenWords =
    sentenceHiddenWords
        |> List.sortBy .sortKey
        |> List.map .answer


playerChoiceToString : PlayerChoice -> String
playerChoiceToString (PlayerChoice wordString) =
    wordString


answerToString : Answer -> String
answerToString (Answer wordString) =
    wordString


wordToString : Word -> String
wordToString word =
    case word of
        HiddenWrd hiddenWord ->
            answerToString hiddenWord.answer

        SentenceWrd wordString ->
            wordString


view : Model -> Html Msg
view model =
    main_ [ class "section" ]
        [ div [ class "container" ]
            [ viewTitle
            , div [ class "box" ]
                [ viewOriginalSentence model.sentence
                , viewSentence (hiddenWords model.sentence) model.sentence
                , viewHiddenWords (hiddenWords model.sentence)
                ]
            ]
        ]


viewOriginalSentence : Words -> Html Msg
viewOriginalSentence words =
    p
        [ class "has-text-centered" ]
        (List.intersperse (text " ") (List.map (wordToString >> text) words))


viewSentence : List HiddenWord -> Words -> Html Msg
viewSentence hiddenSentenceWords sentence =
    let
        viewIndexWord : Int -> Word -> Html Msg
        viewIndexWord index sentenceWord =
            viewWord hiddenSentenceWords index sentenceWord
    in
    div [ class "has-text-centered" ] <|
        List.indexedMap viewIndexWord sentence


viewWord : List HiddenWord -> Int -> Word -> Html Msg
viewWord hiddenSentenceWords index word =
    case word of
        HiddenWrd hiddenWord ->
            viewHiddenWord hiddenSentenceWords index hiddenWord

        SentenceWrd value ->
            viewSentenceWord value


viewSentenceWord : String -> Html Msg
viewSentenceWord value =
    span [ class "sentence-word" ] [ text value ]


viewHiddenWord : List HiddenWord -> Int -> HiddenWord -> Html Msg
viewHiddenWord sentenceHiddenWords index hiddenWord =
    let
        viewOption : PlayerChoice -> Answer -> Html Msg
        viewOption (PlayerChoice choice) (Answer answer) =
            option
                [ value answer, selected (choice == answer) ]
                [ text <| String.toLower answer ]
    in
    div [ class "select" ]
        [ select [ class "hidden-word", onInput (WordChanged index) ] <|
            option []
                [ text "" ]
                :: List.map (viewOption hiddenWord.playerChoice) (answers sentenceHiddenWords)
        ]


viewHiddenWords : List HiddenWord -> Html msg
viewHiddenWords chosenWords =
    let
        hiddenWordsSorted : List HiddenWord
        hiddenWordsSorted =
            List.sortBy .sortKey chosenWords

        viewHiddenWord_ chosenWord =
            let
                answerString : String
                answerString =
                    answerToString chosenWord.answer

                playerChoiceString : String
                playerChoiceString =
                    playerChoiceToString chosenWord.playerChoice

                isCorrectGuess : Bool
                isCorrectGuess =
                    playerChoiceString == answerString

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
    ul [] <|
        List.map viewHiddenWord_ hiddenWordsSorted


viewTitle : Html msg
viewTitle =
    h1 [ class "title has-text-centered" ]
        [ text "Words Memory Game" ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }
