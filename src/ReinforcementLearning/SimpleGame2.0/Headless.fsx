#load "Game.fs"
#load "Brains.fs"

open System
open SimpleGame.Game
open SimpleGame.Brains

// world initialization
let size = { Width = 40; Height = 20 }
let player = { Position = { Top = 10; Left = 20 }; Direction = North }

let rng = Random ()
let board = Array2D.init size.Width size.Height (fun left top ->
    rng.Next(tileValues.Length))

let score = 0
let initialGameState = { Board = board; Hero = player; Score = score }

let simulate (decide:Brain -> State -> Act) iters runs =
    
    // Loop now includes the iteration count
    let rec loop (state:GameState,brain:Brain,iter:int) =

        let currentState = visibleState size state.Board state.Hero
        let decision = decide brain currentState

        // world update
        let player = state.Hero |> applyDecision size decision
        let board = updateBoard state.Board player
        let gain = computeGain state.Board player
        let score = state.Score + gain

        // learning
        let nextState = visibleState size board player
        let experience = {
            State = currentState;
            Action = decision;
            Reward = gain |> float;
            NextState = nextState; }
        let brain = learn brain experience

        let updated = { Board = board; Hero = player; Score = score }

        if iter < iters
        then loop (updated,brain,iter+1)
        else score
    
    [ for run in 1 .. runs -> loop (initialGameState,Map.empty,0) ]

// Simulating different brains
printfn "Random decision"
let random = simulate (fun _ _ -> randomDecide ()) 500 20
printfn "Average score: %.0f" (random |> Seq.averageBy float)

printfn "Crude brain"
let crudeBrain = simulate SimpleGame.Brains.decide 500 20
printfn "Average score: %.0f" (crudeBrain |> Seq.averageBy float)
