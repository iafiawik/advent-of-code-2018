module DayXX.Tests exposing (suite)

import DayXX.DayXX as DayXX exposing (..)
import Expect exposing (Expectation)
import List.Extra exposing (getAt)
import Test exposing (..)


suite : Test
suite =
    describe "DayXX"
        [ describe "count occurrences of character"
            [ test "when there is one occurance" <|
                \_ ->
                    Expect.equal
                        (countOccurrences "a" "abcdef")
                        1
            , test "when there is multiple occurrences" <|
                \_ ->
                    Expect.equal
                        (countOccurrences "a" "abadea")
                        3
            , test "when there are no occurrences" <|
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
        ]
