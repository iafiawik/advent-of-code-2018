module Day03.Tests exposing (suite)

import Day03.Day03 as Day03 exposing (calculateTotalFabricSize, claimSpace, countSquareInchesOfOverlappingClaims, generateFabric, parseInstruction)
import Expect exposing (Expectation)
import List.Extra exposing (getAt)
import Test exposing (..)


suite : Test
suite =
    describe "Day03"
        [ describe "parseInstruction"
            [ test "parses an instruction correctly" <|
                \_ ->
                    let
                        instruction =
                            parseInstruction "#1 @ 906,735: 28x17"

                        assertion =
                            { id = "1"
                            , x = 906
                            , y = 735
                            , width = 28
                            , height = 17
                            }
                    in
                    Expect.equal
                        instruction
                        assertion
            ]
        , describe "calculateTotalFabricSize"
            [ test "for a list of instructions" <|
                \_ ->
                    let
                        instructions =
                            [ { id = "#1"
                              , x = 906
                              , y = 0
                              , width = 28
                              , height = 170
                              }
                            , { id = "#2"
                              , x = 0
                              , y = 735
                              , width = 280
                              , height = 17
                              }
                            ]

                        fabricSize =
                            calculateTotalFabricSize
                                instructions
                    in
                    Expect.equal
                        fabricSize
                        ( 934, 752 )
            ]
        , describe "generateFabric"
            [ test "generates correct number of rows for a given dimension" <|
                \_ ->
                    Expect.equal
                        (List.length
                            (generateFabric
                                ( 10, 20 )
                            ).rows
                        )
                        20
            , test "generates correct number of columns for a given dimension" <|
                \_ ->
                    let
                        fabric =
                            generateFabric
                                ( 10, 20 )

                        row =
                            Maybe.withDefault [] (getAt 0 fabric.rows)
                    in
                    Expect.equal
                        (List.length
                            row
                        )
                        fabric.width
            ]
        , describe "claimSpace"
            [ test "claims the correct space for a specific claim" <|
                \_ ->
                    let
                        claim1 =
                            { id = "#1"
                            , x = 1
                            , y = 3
                            , width = 4
                            , height = 4
                            }

                        claim2 =
                            { id = "#2"
                            , x = 3
                            , y = 1
                            , width = 4
                            , height = 4
                            }

                        claim3 =
                            { id = "#3"
                            , x = 5
                            , y = 5
                            , width = 2
                            , height = 2
                            }

                        instructions =
                            [ claim1
                            , claim2
                            , claim3
                            ]

                        fabricSize =
                            calculateTotalFabricSize
                                instructions

                        fabric =
                            generateFabric
                                ( Tuple.first fabricSize, Tuple.second fabricSize )

                        fabricWithClaimedSpace1 =
                            claimSpace claim1 fabric

                        fabricWithClaimedSpace2 =
                            claimSpace claim2 fabricWithClaimedSpace1

                        fabricWithClaimedSpace3 =
                            claimSpace claim3 fabricWithClaimedSpace2
                    in
                    Expect.equal 20
                        20
            ]
        , describe "countSquareInchesOfOverlappingClaims"
            [ test "generates correct number of rows for a given dimension" <|
                \_ ->
                    let
                        claim1 =
                            { id = "#1"
                            , x = 1
                            , y = 3
                            , width = 4
                            , height = 4
                            }

                        claim2 =
                            { id = "#2"
                            , x = 3
                            , y = 1
                            , width = 4
                            , height = 4
                            }

                        claim3 =
                            { id = "#3"
                            , x = 5
                            , y = 5
                            , width = 2
                            , height = 2
                            }

                        instructions =
                            [ claim1
                            , claim2
                            , claim3
                            ]

                        fabricSize =
                            calculateTotalFabricSize
                                instructions

                        fabric =
                            generateFabric
                                ( Tuple.first fabricSize, Tuple.second fabricSize )

                        fabricWithClaimedSpace1 =
                            claimSpace claim1 fabric

                        fabricWithClaimedSpace2 =
                            claimSpace claim2 fabricWithClaimedSpace1

                        fabricWithClaimedSpace3 =
                            claimSpace claim3 fabricWithClaimedSpace2

                        overlapping =
                            countSquareInchesOfOverlappingClaims fabricWithClaimedSpace3
                    in
                    Expect.equal overlapping
                        4
            ]
        ]
