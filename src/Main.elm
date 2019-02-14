module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, i, li, main_, option, p, select, span, text, ul)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onClick, onInput)
import Random
import Random.List


type Word
    = SentenceWord ( Int, String )
    | HiddenWord ( Int, String )


type alias Words =
    List Word


type alias Model =
    { sentence : String
    , chosenWords : Words
    , chosenSentence : Words
    }


initialModel : Model
initialModel =
    { sentence = "The pen is mightier than the sword"
    , chosenWords = []
    , chosenSentence =
        [ SentenceWord ( 0, "The" )
        , SentenceWord ( 1, "pen" )
        , SentenceWord ( 2, "is" )
        , SentenceWord ( 3, "mightier" )
        , SentenceWord ( 4, "than" )
        , SentenceWord ( 5, "the" )
        , SentenceWord ( 6, "sword" )
        ]
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, chooseWords initialModel.chosenSentence )


type Msg
    = WordChanged Int String
    | WordsChosen Words
    | NewGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WordChanged index wordString ->
            let
                updateWord : Word -> Word
                updateWord word =
                    case word of
                        (HiddenWord ( hiddenIndex, _ )) as hiddenWord ->
                            if hiddenIndex == index then
                                HiddenWord ( index, wordString )

                            else
                                hiddenWord

                        _ ->
                            word

                newSentence : Words
                newSentence =
                    List.map updateWord model.chosenSentence
            in
            ( { model | chosenSentence = newSentence }, Cmd.none )

        WordsChosen words ->
            let
                newSentence =
                    makeChosenSentence model.chosenSentence words

                newChosenWords =
                    makeChosenWords words
            in
            ( { model
                | chosenWords = newChosenWords
                , chosenSentence = newSentence
              }
            , Cmd.none
            )

        NewGame ->
            ( initialModel, chooseWords initialModel.chosenSentence )


makeChosenSentence : Words -> Words -> Words
makeChosenSentence chosenSentence chosenWords =
    List.map
        (\word ->
            case word of
                SentenceWord ( index, w ) ->
                    if List.member word chosenWords then
                        HiddenWord ( index, "" )

                    else
                        word

                _ ->
                    word
        )
        chosenSentence


makeChosenWords : Words -> Words
makeChosenWords chosenWords =
    List.map
        (\word ->
            case word of
                SentenceWord w ->
                    HiddenWord w

                _ ->
                    word
        )
        chosenWords


view : Model -> Html Msg
view model =
    main_ [ class "section" ]
        [ div [ class "container" ]
            [ viewTitle
            , div [ class "box" ]
                [ p
                    [ class "has-text-centered" ]
                    [ text model.sentence ]
                , viewSentence model.chosenSentence model.chosenWords
                , viewChosenWords model.chosenWords model.chosenSentence
                , div [ class "is-clearfix" ]
                    [ button
                        [ class "button is-info is-pulled-right"
                        , onClick NewGame
                        ]
                        [ text "New Game" ]
                    ]
                ]
            ]
        ]


viewSentence : Words -> Words -> Html Msg
viewSentence sentence chosenWords =
    div [ class "has-text-centered" ]
        (List.map
            (\sentenceWord ->
                case sentenceWord of
                    SentenceWord ( _, word ) ->
                        span [ class "sentence-word" ] [ text word ]

                    HiddenWord _ ->
                        viewHiddenWord sentenceWord chosenWords
            )
            sentence
        )


viewHiddenWord : Word -> List Word -> Html Msg
viewHiddenWord hiddenWord chosenWords =
    case hiddenWord of
        HiddenWord ( hiddenIndex, hiddenWordText ) ->
            let
                viewOption : String -> Html Msg
                viewOption wordString =
                    option
                        [ value wordString, selected (wordString == hiddenWordText) ]
                        [ text <| String.toLower wordString ]

                wordElement : Word -> Html Msg
                wordElement word =
                    case word of
                        HiddenWord ( _, wordString ) ->
                            viewOption wordString

                        SentenceWord ( _, wordString ) ->
                            viewOption wordString
            in
            div [ class "select" ]
                [ select [ class "hidden-word", onInput (WordChanged hiddenIndex) ] <|
                    option []
                        [ text "" ]
                        :: List.map wordElement chosenWords
                ]

        _ ->
            text ""


viewChosenWords : Words -> Words -> Html msg
viewChosenWords chosenWords sentenceWords =
    let
        viewChosenWord : Word -> Html msg
        viewChosenWord chosenWord =
            case chosenWord of
                HiddenWord ( _, wordString ) ->
                    let
                        isCorrectGuess : Bool
                        isCorrectGuess =
                            List.member chosenWord sentenceWords

                        className : String
                        className =
                            if isCorrectGuess then
                                "has-text-success"

                            else
                                "has-text-grey-light"
                    in
                    li []
                        [ span [ class className ]
                            [ text wordString
                            , text " "
                            , span [ class "icon is-small" ]
                                [ i [ class "far fa-check-circle" ] [] ]
                            ]
                        ]

                SentenceWord _ ->
                    text ""
    in
    ul [] (List.map viewChosenWord chosenWords)


viewTitle : Html msg
viewTitle =
    h1 [ class "title has-text-centered" ]
        [ text "Words Memory Game" ]



---- COMMANDS ----


chooseWords : Words -> Cmd Msg
chooseWords chosenSentence =
    chosenSentence
        |> Random.List.shuffle
        |> Random.map (List.take 3)
        |> Random.generate WordsChosen



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
