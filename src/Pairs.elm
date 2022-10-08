module Pairs exposing (Pairing(..), fromString, shuffle, toPairs)

import Array
import Random


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


fromString : String -> List String
fromString =
    String.split "\n" >> List.map String.trim >> List.filter (not << String.isEmpty)


shuffle : Random.Seed -> List a -> List a
shuffle seed list =
    shuffleListHelper seed list []


removeAtIndex : Int -> List a -> List a
removeAtIndex index list =
    let
        arr =
            Array.fromList list

        firstSlice =
            Array.slice 0 index arr

        secondSlice =
            Array.slice (index + 1) (List.length list + 1) arr
    in
    Array.toList firstSlice ++ Array.toList secondSlice


shuffleListHelper : Random.Seed -> List a -> List a -> List a
shuffleListHelper seed source result =
    -- based on https://stackoverflow.com/questions/42207900/how-to-shuffle-a-list-in-elm
    if List.isEmpty source then
        result

    else
        let
            indexGenerator =
                Random.int 0 (List.length source - 1)

            ( randomIndex, nextSeed ) =
                Random.step indexGenerator seed

            valAtIndex =
                Array.fromList source |> Array.get randomIndex

            sourceWithoutIndex =
                removeAtIndex randomIndex source
        in
        case valAtIndex of
            Just val ->
                shuffleListHelper nextSeed sourceWithoutIndex (val :: result)

            Nothing ->
                -- we somehow generated an index outside of the list
                -- we just return the unrandomised list
                source
