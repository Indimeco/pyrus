module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, h1, h2, img, label, section, span, text, textarea)
import Html.Attributes exposing (class, placeholder, src)
import Html.Events exposing (onClick, onInput)
import Pairs exposing (Pairing(..), fromString)


type Msg
    = Input String
    | GeneratePairs


type alias Model =
    { input : String
    , pairs : List Pairing
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init =
            { input = ""
            , pairs = []
            }
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input s ->
            { model
                | input = s
            }

        GeneratePairs ->
            { model
                | pairs = fromString model.input
            }


view : Model -> Html Msg
view model =
    let
        headerText =
            if List.isEmpty model.pairs then
                ""

            else
                "Generated Pairs"
    in
    div [ class "main" ]
        [ section [ class "section__inputs" ]
            [ h1 [ class "title" ] [ text "Pair together!" ]
            , label [] [ text "List things to pair" ]
            , textarea [ onInput Input, class "input", placeholder "first\nsecond\nthird" ] []
            , button [ onClick GeneratePairs, class "button__generate" ] [ text "Generate Pairs" ]
            ]
        , section [ class "section__pairs" ]
            [ h2 [ class "header__pairs" ]
                [ text headerText ]
            , section
                [ class "section__pairs__cards" ]
                (List.map
                    pairCard
                    model.pairs
                )
            ]
        ]


pairCard : Pairing -> Html msg
pairCard pair =
    pair
        |> pairCardItem
        |> (\card -> div [ class "paircard" ] [ card ])


pairCardItem : Pairing -> Html msg
pairCardItem pair =
    case pair of
        Pair one two ->
            div [ class "paircard--matched" ] [ text (one ++ " " ++ two) ]

        Unmatched one ->
            div [ class "paircard--unmatched" ] [ span [] [ text ("Unmatched: " ++ one) ], img [ src "sadpear.png" ] [] ]
