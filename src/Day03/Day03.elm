module Day03.Day03 exposing (calculateTotalFabricSize, claimSpace, countSquareInchesOfOverlappingClaims, generateFabric, parseInstruction, resultPart1, resultPart2)

import Day03.Input exposing (getInput)
import List.Extra exposing (getAt, last, maximumWith, setAt, unique)


type alias Instruction =
    { id : String
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    }


resultPart1 : Int
resultPart1 =
    countSquareInchesOfOverlappingClaims getFabricWithClaims


resultPart2 : String
resultPart2 =
    let
        input =
            getInput

        instructions =
            List.map (\rawInput -> parseInstruction rawInput) input

        fabricWithClaims =
            getFabricWithClaims
    in
    findNotOverlappingClaim fabricWithClaims instructions


getFabricWithClaims : Fabric
getFabricWithClaims =
    let
        input =
            getInput

        instructions =
            List.map (\rawInput -> parseInstruction rawInput) input

        fabricSize =
            calculateTotalFabricSize
                instructions

        fabric =
            generateFabric
                ( Tuple.first fabricSize, Tuple.second fabricSize )

        initialState =
            { instructions = instructions, currentIndex = 0, fabric = fabric }

        result =
            while
                (\state -> state.currentIndex <= List.length instructions)
                initialState
                (\state ->
                    let
                        instruction =
                            Maybe.withDefault { id = "", x = 0, y = 0, width = 0, height = 0 } (getAt state.currentIndex state.instructions)

                        claimedFabric =
                            claimSpace instruction state.fabric

                        _ =
                            Debug.log "getFabricWithClaims Instruction" (String.fromInt state.currentIndex ++ " of " ++ String.fromInt (List.length state.instructions))
                    in
                    { instructions = instructions, fabric = claimedFabric, currentIndex = state.currentIndex + 1 }
                )
    in
    result.fabric


parseInstruction : String -> Instruction
parseInstruction instruction =
    let
        parts =
            String.split " " instruction

        id =
            Maybe.withDefault "1" (getAt 1 (String.split "#" (Maybe.withDefault "#1" (getAt 0 parts))))

        positionsString =
            Maybe.withDefault "0,0:" (getAt 2 parts)

        positionsStringLength =
            String.length positionsString

        positionsStringStripped =
            String.slice 0 (positionsStringLength - 1) positionsString

        positions =
            String.split "," positionsStringStripped

        x =
            Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (getAt 0 positions)))

        y =
            Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (getAt 1 positions)))

        dimensions =
            String.split "x" (Maybe.withDefault "x" (getAt 3 parts))

        width =
            Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (getAt 0 dimensions)))

        height =
            Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (getAt 1 dimensions)))
    in
    { id = id
    , x = x
    , y = y
    , width = width
    , height = height
    }


calculateTotalFabricSize : List Instruction -> ( Int, Int )
calculateTotalFabricSize instructions =
    let
        maximumWidth =
            Maybe.withDefault { id = " ", x = 0, width = 0, y = 0, height = 0 }
                (maximumWith
                    (\instructionA instructionB -> compare (instructionA.x + instructionA.width) (instructionB.x + instructionB.width))
                    instructions
                )

        maximumHeight =
            Maybe.withDefault { id = " ", x = 0, width = 0, y = 0, height = 0 }
                (maximumWith
                    (\instructionA instructionB -> compare (instructionA.y + instructionA.height) (instructionB.y + instructionB.height))
                    instructions
                )
    in
    ( maximumWidth.x + maximumWidth.width, maximumHeight.y + maximumHeight.height )


type alias State =
    { width : Int
    , height : Int
    , rows : List (List Claim)
    , columns : List Claim
    }


while : (state -> Bool) -> state -> (state -> state) -> state
while condition initialState body =
    if not (condition initialState) then
        initialState

    else
        while condition (body initialState) body


allRowsHaveNotBeenCreated : State -> Bool
allRowsHaveNotBeenCreated state =
    not (List.length state.rows == state.height)


allColumnsHaveNotBeenCreated : State -> Bool
allColumnsHaveNotBeenCreated state =
    not (List.length state.columns == state.width)


generateFabric : ( Int, Int ) -> Fabric
generateFabric fabricDimensions =
    let
        initialState =
            { width = Tuple.first fabricDimensions, height = Tuple.second fabricDimensions, rows = [], columns = [] }

        grid =
            while
                allRowsHaveNotBeenCreated
                initialState
                (\outerState ->
                    let
                        columns =
                            while
                                allColumnsHaveNotBeenCreated
                                initialState
                                (\innerState ->
                                    { width = innerState.width
                                    , height = innerState.height
                                    , rows = innerState.rows
                                    , columns = { counter = 0, claimIds = [] } :: innerState.columns
                                    }
                                )
                    in
                    { width = outerState.width
                    , height = outerState.height
                    , rows = columns.columns :: outerState.rows
                    , columns = []
                    }
                )
    in
    { rows = grid.rows, width = grid.width, height = grid.height }


type alias ClaimState =
    { currentX : Int
    , currentY : Int
    , instruction : Instruction
    , fabric : Fabric
    }


type alias Fabric =
    { rows : List (List Claim)
    , width : Int
    , height : Int
    }


type alias Claim =
    { counter : Int
    , claimIds : List String
    }


allRowsHaveNotBeenClaimed : ClaimState -> Bool
allRowsHaveNotBeenClaimed state =
    state.currentY - state.instruction.height < state.instruction.y


allColumnsHaveNotBeenClaimed : ClaimState -> Bool
allColumnsHaveNotBeenClaimed state =
    state.currentX - state.instruction.width < state.instruction.x


claimSpace : Instruction -> Fabric -> Fabric
claimSpace instruction fabric =
    let
        initialState =
            { currentX = instruction.x, currentY = instruction.y, instruction = instruction, fabric = fabric }

        grid =
            while
                allRowsHaveNotBeenClaimed
                initialState
                (\outerState ->
                    let
                        columns =
                            while
                                allColumnsHaveNotBeenClaimed
                                outerState
                                (\innerState ->
                                    let
                                        row =
                                            Maybe.withDefault [] (getAt outerState.currentY innerState.fabric.rows)

                                        currentValue =
                                            Maybe.withDefault { counter = 0, claimIds = [] } (getAt innerState.currentX row)

                                        newValue =
                                            { counter = currentValue.counter + 1, claimIds = outerState.instruction.id :: currentValue.claimIds }

                                        newRow =
                                            setAt innerState.currentX newValue row

                                        newFabricRows =
                                            setAt outerState.currentY newRow innerState.fabric.rows

                                        newFabric =
                                            { width = outerState.fabric.width, height = outerState.fabric.height, rows = newFabricRows }
                                    in
                                    { currentX = innerState.currentX + 1
                                    , currentY = outerState.currentY
                                    , instruction = outerState.instruction
                                    , fabric = newFabric
                                    }
                                )
                    in
                    { currentX = outerState.currentX
                    , currentY = outerState.currentY + 1
                    , instruction = outerState.instruction
                    , fabric = columns.fabric
                    }
                )
    in
    grid.fabric


countSquareInchesOfOverlappingClaims : Fabric -> Int
countSquareInchesOfOverlappingClaims fabric =
    let
        rows =
            List.map (\row -> List.length (List.filter (\column -> column.counter > 1) row)) fabric.rows

        numberOfColumns =
            List.foldl (+) 0 rows
    in
    numberOfColumns


type NestedList a
    = Element a
    | Nested (List (NestedList a))


toList : NestedList a -> List a
toList nested =
    let
        recur node acc =
            case node of
                Element a ->
                    a :: acc

                Nested list ->
                    List.foldr recur acc list
    in
    recur nested []


findNotOverlappingClaim : Fabric -> List Instruction -> String
findNotOverlappingClaim fabric instructions =
    let
        allColumns =
            List.concat fabric.rows

        initialState =
            { instructions = instructions, currentIndex = 0, fabric = fabric, claims = [] }

        result =
            while
                (\state -> state.currentIndex < List.length instructions)
                initialState
                (\state ->
                    let
                        instruction =
                            Maybe.withDefault { id = "", x = 0, y = 0, width = 0, height = 0 } (getAt state.currentIndex state.instructions)

                        occupiedColumns =
                            List.filter (\column -> List.any (\claimId -> claimId == instruction.id) column.claimIds) allColumns

                        overlaps =
                            List.all (\column -> column.counter == 1) occupiedColumns

                        claim =
                            { overlaps = not overlaps, claimId = instruction.id }

                        _ =
                            Debug.log "Instruction" (String.fromInt state.currentIndex ++ " of " ++ String.fromInt (List.length state.instructions) ++ " , overlaps: " ++ Debug.toString claim)
                    in
                    { instructions = instructions, fabric = state.fabric, currentIndex = state.currentIndex + 1, claims = claim :: state.claims }
                )

        notOverlappingClaim =
            Maybe.withDefault { overlaps = False, claimId = "Not found" } (getAt 0 (List.filter (\claim -> claim.overlaps == False) result.claims))
    in
    notOverlappingClaim.claimId
