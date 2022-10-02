module Pairs exposing (Pairing(..), fromString, toPairs)


type Pairing
    = Pair String String
    | Unmatched String


toPairs : List String -> List Pairing
toPairs input =
    case input of
        [ a ] ->
            [ Unmatched a ]

        [ a, b ] ->
            [ Pair a b ]

        a :: b :: xs ->
            List.append [ Pair a b ] (toPairs xs)

        _ ->
            []


fromString : String -> List Pairing
fromString =
    String.split "\n" >> List.map String.trim >> List.filter (not << String.isEmpty) >> toPairs
