module Day02.Day02 exposing (calculateCheckSum, countOccurrences, resultPart1, resultPart2, splitBoxId, twiceAndThreesOccurrences)

import Day02.Input exposing (getInput)
import List.Extra exposing (count, getAt, last, unique)


countOccurrences : String -> String -> Int
countOccurrences needle haystack =
    if String.length needle == 0 || String.length haystack == 0 then
        0

    else
        haystack
            |> String.indexes needle
            |> List.length


splitBoxId : String -> List String
splitBoxId boxId =
    String.split "" boxId


twiceAndThreesOccurrences : String -> ( Bool, Bool )
twiceAndThreesOccurrences boxId =
    let
        characters =
            splitBoxId boxId

        characterAndOccurrences =
            List.map (\character -> ( character, countOccurrences character boxId )) characters

        containsTwices =
            List.any (\characterOccurrency -> Tuple.second characterOccurrency == 2) characterAndOccurrences

        containsThrees =
            List.any (\characterOccurrency -> Tuple.second characterOccurrency == 3) characterAndOccurrences

        result =
            ( containsTwices, containsThrees )
    in
    result


calculateCheckSum : List String -> Int
calculateCheckSum boxIds =
    let
        occurances =
            List.map (\boxId -> twiceAndThreesOccurrences boxId) boxIds

        twices =
            count ((==) True) (List.map (\occ -> Tuple.first occ) occurances)

        threes =
            count ((==) True) (List.map (\occ -> Tuple.second occ) occurances)

        _ =
            Debug.log "" (String.fromInt twices)

        _ =
            Debug.log "" (String.fromInt threes)
    in
    twices * threes


resultPart1 : Int
resultPart1 =
    let
        input =
            getInput

        checksum =
            calculateCheckSum input
    in
    checksum


resultPart2 : Int
resultPart2 =
    let
        input =
            getInput
    in
    3
