module PairsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Pairs exposing (Pairing(..), toPairs)
import Random
import Test exposing (..)


suite : Test
suite =
    describe "Pairs"
        [ describe "toPairs"
            -- Nest as many descriptions as you like.
            [ test "makes pairs given a small list" <|
                \_ ->
                    Expect.equal [ Pair "Jack" "Jill" ] <| toPairs [ "Jack", "Jill" ]
            , test "makes pairs given a larger list" <|
                \_ ->
                    Expect.equal [ Pair "Jack" "Jill", Pair "Sam" "Frodo", Pair "Tigger" "Pooh" ] <|
                        toPairs
                            [ "Jack"
                            , "Jill"
                            , "Sam"
                            , "Frodo"
                            , "Tigger"
                            , "Pooh"
                            ]
            , test "makes pairs with an odd number of participants" <|
                \_ ->
                    Expect.equal [ Pair "Jack" "Jill", Unmatched "Gandalf" ] <|
                        toPairs [ "Jack", "Jill", "Gandalf" ]
            ]
        , describe "fromString"
            [ test "makes pairs given a string" <|
                \_ ->
                    Expect.equal [ Pair "Jack" "Jill" ] <| Pairs.fromString "Jack\nJill"
            , test "makes pairs given a complex string" <|
                \_ ->
                    Expect.equal [ Pair "Jack" "Jill", Pair "Sam" "Frodo", Unmatched "Gandalf" ] <| Pairs.fromString "Jack\nJill\nSam\nFrodo\nGandalf"
            , test "makes pairs given a string with bad formatting" <|
                \_ ->
                    Expect.equal [ Pair "Jack" "Jill", Pair "Sam" "Frodo", Unmatched "Gandalf" ] <| Pairs.fromString "Jack\n\n\nJill\nSam \n    Frodo\nGandalf     "
            ]
        , describe "shuffle"
            [ test "does nothing to an empty list" <|
                \_ -> Expect.equal [] <| Pairs.shuffle (Random.initialSeed 1) []
            , test "does nothing to single value list" <|
                \_ -> Expect.equal [ "duck" ] <| Pairs.shuffle (Random.initialSeed 1) [ "duck" ]
            , test "randomises a small list in one order" <|
                \_ -> Expect.equal [ 1, 2 ] <| Pairs.shuffle (Random.initialSeed 1) [ 1, 2 ]
            , test "randomises a small list in a different order" <|
                \_ -> Expect.equal [ 2, 1 ] <| Pairs.shuffle (Random.initialSeed 2) [ 1, 2 ]
            ]
        ]
