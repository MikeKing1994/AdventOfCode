namespace Project
module Program = 
    open System

    let split = Calories.input.Split("\r\n\r\n", StringSplitOptions.RemoveEmptyEntries)
    
    let tryParseInt (i: string) = 
        match Int32.TryParse i with 
        | true, parsed -> parsed 
        | _ -> 0

    let toCalorieCount elf = 
        elf 
        |> Array.map tryParseInt
        |> Array.sum

    let splitPerElf (elf: string) = 
        elf.Split("\r\n")

    split 
    |> Array.where (fun elf -> String.IsNullOrWhiteSpace(elf) |> not)
    |> Array.map splitPerElf
    |> Array.map toCalorieCount
    |> Array.sortDescending
    |> Array.take 3
    |> Array.sum
    |> printfn "%i"

