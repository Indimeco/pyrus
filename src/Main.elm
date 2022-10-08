module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, a, button, div, h1, h2, img, label, section, span, text, textarea)
import Html.Attributes exposing (class, href, placeholder, src)
import Html.Events exposing (onClick, onInput)
import Pairs exposing (Pairing(..), fromString, shuffle, toPairs)
import Random


type Msg
    = Input String
    | GeneratePairs


type alias Model =
    { input : String
    , pairs : List Pairing
    , seed : Random.Seed
    }


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init currentTime =
    ( { input = ""
      , pairs = []
      , seed = Random.initialSeed currentTime
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Input s ->
            ( { model
                | input = s
              }
            , Cmd.none
            )

        GeneratePairs ->
            ( { model
                | pairs = shuffle model.seed (fromString model.input) |> toPairs
                , seed = Random.step (Random.int Random.minInt Random.maxInt) model.seed |> (\( _, newSeed ) -> newSeed)
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view model =
    let
        headerText =
            if List.isEmpty model.pairs then
                ""

            else
                "Generated Pairs"
    in
    div []
        [ img [ class "hero", src "pear_hero.jpg" ] []
        , div
            [ class "main" ]
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
        , div [ class "footer" ] [ a [ href "https://github.com/Indimeco/pyrus" ] [ text "Pluck the source code on github" ] ]
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
