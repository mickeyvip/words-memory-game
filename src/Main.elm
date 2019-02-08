module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, main_, option, p, select, span, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)


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
    , chosenWords =
        [ HiddenWord ( 1, "pen" )
        , HiddenWord ( 6, "sword" )
        , HiddenWord ( 3, "mightier" )
        ]
    , chosenSentence =
        [ SentenceWord ( 0, "The" )
        , HiddenWord ( 1, "" )
        , SentenceWord ( 2, "is" )
        , HiddenWord ( 3, "" )
        , SentenceWord ( 4, "than" )
        , SentenceWord ( 5, "the" )
        , HiddenWord ( 6, "" )
        ]
    }


type Msg
    = WordChanged Int String


update : Msg -> Model -> Model
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
            { model | chosenSentence = newSentence }


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
