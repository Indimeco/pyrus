module PairsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Pairs exposing (toPairs)
import Test exposing (..)


suite : Test
suite =
    describe "Pairs"
        [ describe "toPairs"
            -- Nest as many descriptions as you like.
            [ test "makes pairs given a small list" <|
                \_ ->
                    Expect.equal [ ( "Jack", "Jill" ) ] toPairs [ "Jack", "Jill" ]
            ]
        ]
