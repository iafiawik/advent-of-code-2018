module Day02.Tests exposing (suite)

import Day02.Day02 as Day02 exposing (calculateCheckSum, countOccurrences, splitBoxId, twiceAndThreesOccurrences)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Extra exposing (getAt, last, uniqueBy)
import Test exposing (..)


suite : Test
suite =
    describe "Day02"
        [ describe "count occurencies of character"
            [ test "when there is one occurance" <|
                \_ ->
                    Expect.equal
                        (countOccurrences "a" "abcdef")
                        1
            , test "when there is multiple occurancies" <|
                \_ ->
                    Expect.equal
                        (countOccurrences "a" "abadea")
                        3
            , test "when there are no occurancies" <|
                \_ ->
                    Expect.equal
                        (countOccurrences "z" "abadea")
                        0
            , test "for a list of characters" <|
                \_ ->
                    let
                        boxId =
                            "aaaaab"

                        characters =
                            splitBoxId boxId

                        characterAndOccurrences =
                            List.map (\character -> ( character, countOccurrences character boxId )) characters

                        firstCharacter =
                            Maybe.withDefault ( "a", 1 ) (getAt 0 characterAndOccurrences)
                    in
                    Expect.equal
                        (Tuple.second firstCharacter)
                        5
            ]
        , describe "splits string correctly"
            [ test "and returns correct number of elements" <|
                \_ ->
                    Expect.equal (List.length (splitBoxId "abcdef"))
                        6
            , test "and places one character at each index" <|
                \_ ->
                    Expect.equal
                        (Maybe.withDefault
                            "0"
                            (getAt 3 (splitBoxId "abcdef"))
                        )
                        "d"
            ]
        , describe "findNumberOfTwices"
            -- Nest as many descriptions as you like.
            [ test
                "returns False when there are none"
              <|
                \_ ->
                    Expect.equal (Tuple.first (twiceAndThreesOccurrences "abcdef")) False
            , test
                "returns True when there is one"
              <|
                \_ ->
                    Expect.equal (Tuple.first (twiceAndThreesOccurrences "aabcde")) True
            , test
                "returns True when there are multiples"
              <|
                \_ ->
                    Expect.equal (Tuple.first (twiceAndThreesOccurrences "aabcce")) True
            ]
        , describe "calculate checksum"
            [ test
                "returns correct checksum for a boxId"
              <|
                \_ ->
                    let
                        checksum =
                            calculateCheckSum [ "abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab" ]
                    in
                    Expect.equal checksum 12
            ]
        ]