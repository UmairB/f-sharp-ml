namespace SimpleGame

open System
open System.Threading
open Game
open Rendering

module Program =
    // world initialization
    let size = { Width = 40; Height = 20 }
    let player = { Position = { Top = 10; Left = 20 }; Direction = North }
    
    let rng = Random ()

    let board = 
        [   for top in 0 .. size.Height - 1 do
                for left in 0 .. size.Width - 1 do
                    if rng.NextDouble () > 0.5
                    then
                        let pos = { Top = top; Left = left }
                        let cell = if rng.NextDouble () > 0.5 then Trap else Treasure
                        yield pos, cell ]
        |> Map.ofList

    let score = 0
    let initialGameState = { Board = board; Hero = player; Score = score }

    // decision function
    let choices = [| Straight; Left; Right; |]
    let decide () = choices.[rng.Next(3)]

    [<EntryPoint>]
    let main argv =

        let rec loop (state:GameState) =
            
            let decision = decide ()

            // world update
            let player = state.Hero |> applyDecision size decision
            let board = updateBoard state.Board player
            let gain = computeGain state.Board player
            let score = state.Score + gain

            // world rendering
            //printfn "%i" score
            renderScore score
            renderPlayer state.Hero player
            renderBoard state.Board board

            let updated = { Board = board; Hero = player; Score = score }

            Thread.Sleep 20
            loop (updated)

        // start the game
        loop (initialGameState) |> ignore

        0 // return an integer exit code
