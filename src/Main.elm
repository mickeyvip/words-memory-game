module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, main_, p, text)
import Html.Attributes exposing (class)


type alias Model =
    { sentence : String
    }


initialModel : Model
initialModel =
    { sentence = "The pen is mightier than the sword" }


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html msg
view model =
    main_ [ class "section" ]
        [ div [ class "container" ]
            [ viewTitle
            , div [ class "box" ]
                [ p
                    [ class "has-text-centered" ]
                    [ text model.sentence ]
                ]
            ]
        ]


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
