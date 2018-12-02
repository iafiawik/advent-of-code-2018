module Day02.Day02 exposing (calculateCheckSum, countOccurrences, findCorrectBoxIds, getNumberOfDifferingCharacters, resultPart1, resultPart2, splitBoxId, stripBoxIdsFromDifferingCharacters, twiceAndThreesOccurrences)

import Day02.Input exposing (getInput)
import List.Extra exposing (count, find, getAt, last, unique)


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
    in
    twices * threes


getCharacterDifferences : ( String, String ) -> List ( Int, Bool )
getCharacterDifferences strings =
    let
        firstSplitted =
            splitBoxId (Tuple.first strings)

        secondSplitted =
            splitBoxId (Tuple.second strings)

        matches =
            List.indexedMap
                (\index firstCharacter ->
                    let
                        secondCharacter =
                            Maybe.withDefault "" (getAt index secondSplitted)
                    in
                    ( index, secondCharacter == firstCharacter )
                )
                firstSplitted
    in
    matches


getNumberOfDifferingCharacters : ( String, String ) -> Int
getNumberOfDifferingCharacters strings =
    let
        numberOfDiffering =
            getCharacterDifferences strings
    in
    count ((==) False) (List.map (\difference -> Tuple.second difference) numberOfDiffering)


compareStrings : String -> List String -> List ( ( String, String ), Int )
compareStrings string allStrings =
    List.map
        (\compareString -> ( ( string, compareString ), getNumberOfDifferingCharacters ( string, compareString ) ))
        allStrings


findCorrectBoxIds : List String -> ( String, String )
findCorrectBoxIds boxIds =
    let
        allStrings =
            List.map (\boxId -> compareStrings boxId boxIds) boxIds

        bestMatchMaybe =
            find
                (\match ->
                    let
                        numberOfDifferingCharacters =
                            Tuple.second match
                    in
                    numberOfDifferingCharacters == 1
                )
                (List.concat allStrings)
    in
    case bestMatchMaybe of
        Just bestMatch ->
            Tuple.first bestMatch

        Nothing ->
            ( "", "" )


stripBoxIdsFromDifferingCharacters : ( String, String ) -> String
stripBoxIdsFromDifferingCharacters strings =
    let
        firstSplitted =
            splitBoxId (Tuple.first strings)

        differences =
            getCharacterDifferences strings

        characterIndexesToKeep =
            List.filter (\difference -> Tuple.second difference) differences

        lettersToKeep =
            List.map (\difference -> Maybe.withDefault "" (getAt (Tuple.first difference) firstSplitted))
                characterIndexesToKeep
    in
    String.join "" lettersToKeep


findCommonLettersBetweenCorrectBoxIds : List String -> String
findCommonLettersBetweenCorrectBoxIds boxIds =
    stripBoxIdsFromDifferingCharacters (findCorrectBoxIds boxIds)


resultPart1 : String
resultPart1 =
    let
        input =
            getInput

        checksum =
            calculateCheckSum input
    in
    "Checksum is: " ++ String.fromInt checksum


resultPart2 : String
resultPart2 =
    let
        result =
            findCommonLettersBetweenCorrectBoxIds getInput
    in
    "Common letters between correct box IDs are: " ++ result
