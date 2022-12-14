open System 

type Fill = 
    | Sand
    | Rock
    | Air

type Point = 
    {
        X: int
        Y: int 
        Fill: Fill
    }

module Point = 
    let rock x y = 
        {
            X = x
            Y = y
            Fill = Rock
        }

    let sand x y = 
        {
            X = x
            Y = y
            Fill = Sand
        }

type Coord = 
    {
        X: int
        Y: int 
    }

module Coord = 
    let create x y = 
        {
            X = x
            Y = y  
        }

let input = """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"""

module Parse = 
    let splitSeams (input: string) = 
        input.Split("\r\n")

    let parseCoord (coord: string) = 
        let coords = coord.Split(',')
        {
            X = coords.[0] |> Int32.Parse
            Y = coords.[1] |> Int32.Parse
        }


    let parseSeam (seam: string) = 
        seam.Split(" -> ")
        |> Array.map (parseCoord)

    type ParseState = 
        {
            Current: Coord
            Rocks: Point list
        }

    module ParseState = 
        let initial first = 
            {
                Current = first 
                Rocks = [ Point.rock first.X first.Y]
            }

    let toRocks (coords: Coord array) = 
        coords.[1..coords.Length-1] 
        |> Array.fold (fun (state: ParseState) coord -> 
            let mutable newRocks : Point list = []
            match state.Current, coord with 
            | current, co when current.X = co.X -> 
                if current.Y > co.Y then 
                    for y in [1..current.Y - co.Y] do
                        //printfn "y's are different, my array is %A" [1..current.Y - co.Y]
                        //printfn "my base Y value is %i" current.Y
                        newRocks <- (Point.rock co.X (current.Y - y))::newRocks
                    { state with 
                        Current = coord 
                        Rocks = state.Rocks@newRocks}
                else 
                    for y in [1..co.Y - current.Y] do
                        //printfn "y's are different, my array is %A" [1..co.Y - current.Y]
                        //printfn "my base Y value is %i" co.Y
                        newRocks <- (Point.rock co.X (current.Y + y))::newRocks
                    { state with 
                        Current = coord 
                        Rocks = state.Rocks@newRocks}

            | current, co when current.Y = co.Y -> 
                if current.X > co.X then 
                    for x in [1..current.X - co.X] do
                        //printfn "x's are different, my array is %A" [1..current.X - co.X]
                        //printfn "my base X value is %i" current.X
                        newRocks <- (Point.rock (current.X - x) co.Y)::newRocks
                    { state with 
                        Current = coord 
                        Rocks = state.Rocks@newRocks}
                else 
                    for x in [1..co.X - current.X] do
                        //printfn "x's are different, my array is %A" [1..co.X - current.X]
                        //printfn "my base X value is %i" co.X
                        newRocks <- (Point.rock (current.X + x) co.Y)::newRocks
                    { state with 
                        Current = coord 
                        Rocks = state.Rocks@newRocks}
            | _ -> failwith "unexpected")
            (ParseState.initial coords.[0])

    let parse (input: string) = 
        input 
        |> splitSeams 
        |> Array.map parseSeam         
        |> Array.map toRocks
        |> Array.collect (fun x -> x.Rocks |> List.toArray)

module FallingSand = 
    let coordIsBlocked (coord: Coord) (model: Point list) = 
        //printfn "CheckingBlock on if X: %i, Y: %i is blocked" coord.X coord.Y
        //printfn "Check yielded %A" (model |> List.exists (fun c -> c.X = coord.X && c.Y = coord.Y))
        model |> List.exists (fun c -> c.X = coord.X && c.Y = coord.Y)

    let canGoDown (coord: Coord) (model: Point list) = 
        //printfn "testing if X: %i, Y: %i is blocked" coord.X (coord.Y + 1)
        coordIsBlocked ({ coord with Y = coord.Y + 1 }) model |> not

    let canGoDownLeft (coord: Coord) (model: Point list) = 
        coordIsBlocked 
            ({ coord with
                X = coord.X - 1
                Y = coord.Y + 1 
            }) 
            model
        |> not

    let canGoDownRight (coord: Coord) (model: Point list) = 
        coordIsBlocked 
            ({ coord with
                X = coord.X + 1
                Y = coord.Y + 1 
            }) 
            model
        |> not

    let dropSand (model: Point list) = 
        let mutable x = 0 
        let mutable y = 0
        let mutable carryOn = true

        while carryOn do 
            if canGoDown (Coord.create (500 - x) (0 + y)) model then 
                //printfn "could go down"
                y <- y + 1
            elif canGoDownLeft (Coord.create (500 - x) (0 + y)) model then do 
                y <- y + 1
                x <- x - 1
            elif canGoDownRight (Coord.create (500 - x) (0 + y)) model then 
                y <- y + 1
                x <- x + 1
            else 
                carryOn <- false

        printfn "dropping sand onto %A" (Point.sand (500 - x) (0 + y))
        (Point.sand (500 - x) (0 + y))::model



        
let startingModel = 
    input 
    |> Parse.parse
    |> Array.toList

let withTenSand = 
    [0..10]
    |> List.fold (fun state _ -> FallingSand.dropSand state) startingModel

printfn "%A" withTenSand

    