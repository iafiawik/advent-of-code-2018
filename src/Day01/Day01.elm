module Day01.Day01 exposing (resultPart1, resultPart2)

import Day01.Input exposing (getInput)
import List.Extra exposing (getAt, last, unique)


resultPart1 : Int
resultPart1 =
    let
        input =
            getInput
    in
    List.sum input



--- Part 2 ---


type alias State =
    { frequencies : List Int
    , input : List Int
    , iterations : Int
    , currentIndex : Int
    }


while : (State -> Bool) -> State -> (State -> State) -> State
while condition initialState body =
    if not (condition initialState) then
        initialState

    else
        while condition (body initialState) body


frequencyHasNotBeenReached : State -> Bool
frequencyHasNotBeenReached state =
    let
        uniques =
            unique state.frequencies
    in
    List.length uniques == List.length state.frequencies


resultPart2 : Int
resultPart2 =
    let
        input =
            getInput

        initialState =
            { frequencies = [], input = input, iterations = 0, currentIndex = 0 }

        result =
            while frequencyHasNotBeenReached
                initialState
                (\state ->
                    let
                        currentChange =
                            Maybe.withDefault 0 (getAt state.currentIndex state.input)

                        currentSum =
                            Maybe.withDefault 0 (last state.frequencies)

                        nextFrequency =
                            currentSum + currentChange

                        frequencies =
                            List.concat [ state.frequencies, [ nextFrequency ] ]
                    in
                    { frequencies = frequencies
                    , input = state.input
                    , iterations = state.iterations + 1
                    , currentIndex =
                        if state.currentIndex + 1 == List.length state.input then
                            0

                        else
                            state.currentIndex + 1
                    }
                )
    in
    Maybe.withDefault 0 (last result.frequencies)
