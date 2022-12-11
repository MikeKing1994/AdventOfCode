open System

let splitIntoRucksacks (str: string) = 
    str.Split("\r\n")

let splitIntoCompartments (rucksack: string) = 
    let compartmentLength = rucksack.Length/2 // Every rucksack is even?
    rucksack.[0..compartmentLength], rucksack.[compartmentLength..rucksack.Length]

let splitIntoGroupsOfThree (allRucksacks: string[]) = 
    [| 0..allRucksacks.Length/3 - 1|]
    |> Array.map (fun baseIndex -> [| allRucksacks.[3*baseIndex]; allRucksacks.[3*baseIndex + 1]; allRucksacks.[3*baseIndex + 2];|])

let rec identifySharedItem (a: string) (b: string) = 
    if a.[0] |> b.Contains then 
        a.[0]  
    else 
        identifySharedItem a.[1..a.Length] b

let rec identifySharedItemBetweenThree (a: string) (b: string) (c: string) = 
    if a.[0] |> b.Contains && a.[0] |> c.Contains then 
        a.[0]  
    else 
        identifySharedItemBetweenThree a.[1..a.Length] b c

let toPriority (c: char) = 
    let position = (int)c % 32;
    if Char.IsUpper c then 
        position + 26 
    else 
        position


Input.input 
|> splitIntoRucksacks
|> Array.map splitIntoCompartments
|> Array.map (fun (a, b) -> identifySharedItem a b)
|> Array.map toPriority
|> Array.sum
|> printfn "%i"

Input.input 
|> splitIntoRucksacks
|> splitIntoGroupsOfThree
|> Array.map (fun group -> identifySharedItemBetweenThree group.[0] group.[1] group.[2])
|> Array.map toPriority
|> Array.sum
|> printfn "%i"