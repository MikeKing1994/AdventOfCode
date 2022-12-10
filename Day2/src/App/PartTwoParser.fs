module PartTwoParser 
    open Domain 

    let splitString (str: string) = 
        str.Split("\r\n")

    let parseOpponent (c: char) = 
        match c with 
        | 'A' -> Rock
        | 'B' -> Paper 
        | 'C' -> Scissor
        | unrecognised -> failwithf "failed to parse letter %A" unrecognised

    let parseResult (c: char) = 
        match c with 
        | 'X' -> Result.ILost
        | 'Y' -> Result.Draw
        | 'Z' -> Result.IWon
        | unrecognised -> failwithf "failed to parse letter %A" unrecognised

    let parseGame (str: string) = 
        let opponent = str.[0] |> parseOpponent
        let result = str.[2] |> parseResult

        let me = 
            match result, opponent with 
            | Draw, opp -> opp
            | ILost, Rock -> Scissor
            | ILost, Paper -> Rock
            | ILost, Scissor -> Paper 
            | IWon, Rock -> Paper 
            | IWon, Paper -> Scissor
            | IWon, Scissor -> Rock
        {
            Opponent = opponent
            Me = me
        }

    let parse (input: string) = 
        input 
        |> splitString
        |> Array.map parseGame
        |> Array.toList
        |> Plan