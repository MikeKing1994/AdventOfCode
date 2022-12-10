module Parser 
    open System
    open Domain

    let splitString (str: string) = 
        str.Split("\r\n")

    let parseLetter (c: char) = 
        match c with 
        | 'A' -> Rock
        | 'X' -> Rock
        | 'B' -> Paper 
        | 'Y' -> Paper
        | 'C' -> Scissor
        | 'Z' -> Scissor
        | unrecognised -> failwithf "failed to parse letter %A" unrecognised

    let parseGame (str: string) = 
        {
            Opponent = str.[0] |> parseLetter
            Me = str.[2] |> parseLetter
        }

    let parse (input: string) = 
        input 
        |> splitString
        |> Array.map parseGame
        |> Array.toList
        |> Plan