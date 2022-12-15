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
                        newRocks <- (Point.rock co.X (current.Y - y))::newRocks
                    { state with 
                        Current = coord 
                        Rocks = state.Rocks@newRocks}
                else 
                    for y in [1..co.Y - current.Y] do
                        newRocks <- (Point.rock co.X (current.Y + y))::newRocks
                    { state with 
                        Current = coord 
                        Rocks = state.Rocks@newRocks}

            | current, co when current.Y = co.Y -> 
                if current.X > co.X then 
                    for x in [1..current.X - co.X] do
                        newRocks <- (Point.rock (current.X - x) co.Y)::newRocks
                    { state with 
                        Current = coord 
                        Rocks = state.Rocks@newRocks}
                else 
                    for x in [1..co.X - current.X] do
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
        model |> List.exists (fun c -> c.X = coord.X && c.Y = coord.Y)

    let canGoDown (coord: Coord) (model: Point list) = 
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

    type DropResult = 
        | Success of Point list
        | Abyss

    let dropSand (model: Point list) = 
        let mutable x = 0 
        let mutable y = 0
        let mutable carryOn = true
        let abyssConstant = 200
        let mutable brokeDueToAbyss = false

        while carryOn do 
            if y > abyssConstant then 
                brokeDueToAbyss <- true
                carryOn <- false
            if canGoDown (Coord.create (500 + x) (0 + y)) model then 
                y <- y + 1
            elif canGoDownLeft (Coord.create (500 + x) (0 + y)) model then 
                y <- y + 1
                x <- x - 1
            elif canGoDownRight (Coord.create (500 + x) (0 + y)) model then 
                y <- y + 1
                x <- x + 1
            else 
                carryOn <- false

        if not brokeDueToAbyss then 
            (Point.sand (500 + x) (0 + y))::model |> DropResult.Success
        else 
            DropResult.Abyss

module Print = 
    let print (model: Point list) = 
        for y in [0..40] do 
            let mutable chars: char list = []
            for x in [450..550] do
            
                let coord = Coord.create x y 
                match model |> List.tryFind (fun x -> x.X = coord.X && x.Y = coord.Y) with 
                | None -> chars <-  ['_'] |> List.append chars
                | Some p when p.Fill = Fill.Rock -> chars <- ['#'] |> List.append chars
                | Some p when p.Fill = Fill.Sand -> chars <- ['O'] |> List.append chars
                | Some p when p.Fill = Fill.Air -> chars <- ['_'] |> List.append chars
                | _ -> failwith "ahh"
            printfn "%A" chars


        
let startingModel = 
    input 
    |> Parse.parse
    |> Array.toList

let withTenSand = 
    let mutable carryOn = true 
    let mutable state: Point list = startingModel
    Print.print state

    
    while carryOn do 
        for i in [0..24] do 
            //Print.print state
            match FallingSand.dropSand state with 
            | FallingSand.DropResult.Success points -> state <- points
            | FallingSand.DropResult.Abyss -> 
                printfn "terminated after i iterations: %i" i
                carryOn <- false
            


    