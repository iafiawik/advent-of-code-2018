module Day01.Day01 exposing (resultPart1)

import Day01.Input exposing (getInput)


resultPart1 : Int
resultPart1 =
    let
        input =
            getInput
    in
    List.sum input
