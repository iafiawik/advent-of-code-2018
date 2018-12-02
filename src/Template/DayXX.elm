module DayXX.DayXX exposing (resultPart1, resultPart2)

import DayXX.Input exposing (getInput)
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
