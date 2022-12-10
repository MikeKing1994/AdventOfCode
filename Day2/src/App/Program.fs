module Program 

Input.input
|> Parser.parse
|> Domain.PlanModule.toTotalScore
|> printfn "%i"

Input.input
|> PartTwoParser.parse
|> Domain.PlanModule.toTotalScore
|> printfn "%i"