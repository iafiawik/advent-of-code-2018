module Day03.Day03 exposing (resultPart1, resultPart2)

import Day03.Input exposing (getInput)
import List.Extra exposing (getAt, last, unique)


resultPart1 : Int
resultPart1 =
    let
        input =
            getInput
    in
    List.sum input


resultPart2 : Int
resultPart2 =
    let
        input =
            getInput
    in
    List.sum input
