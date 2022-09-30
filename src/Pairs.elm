module Pairs exposing (Pair, toPairs)


type alias Pair a =
    ( a, a )


toPair : a -> a -> Pair a
toPair first second =
    ( first, second )


toPairs : List a -> List (Pair a)
toPairs input =
    List.partition (\v -> v remainderBy 2 == 0) input
        |> (\x -> x)
