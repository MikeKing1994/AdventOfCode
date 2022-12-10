module Domain 
type Result = 
    | IWon
    | Draw
    | ILost

module Result = 
    let toScore =
        function 
        | IWon -> 6 
        | Draw -> 3
        | ILost -> 0

type Choice = 
    | Rock
    | Paper
    | Scissor

module Choice = 
    let toScore = 
        function 
        | Rock -> 1
        | Paper -> 2
        | Scissor -> 3

    let toResult opponent me = 
        match opponent, me with 
        | opp, me when opp = me -> Draw
        | Rock, Paper -> IWon
        | Paper, Scissor -> IWon 
        | Scissor, Rock -> IWon 
        | _ -> ILost

type Game = 
    { 
        Opponent: Choice
        Me: Choice
    }

module Game = 
    let toResult game = Choice.toResult game.Opponent game.Me

    let toScore game = Choice.toScore game.Me + (game |> toResult |> Result.toScore)

type Plan = Plan of Game list 

module PlanModule = 
    let unwrap = 
        function 
        | Plan games -> games 

    let toTotalScore (plan: Plan) = 
        plan 
        |> unwrap
        |> List.map Game.toScore
        |> List.sum


//let example: Plan = 
//    [
//        {
//            Opponent = Rock
//            Me = Paper 
//        }
//        {
//            Opponent = Paper
//            Me = Rock
//        }
//        {
//            Opponent = Scissor
//            Me = Scissor 
//        }
//    ] |> Plan
//
//let ret = example |> Plan.toTotalScore
//
//printfn "%i" ret

