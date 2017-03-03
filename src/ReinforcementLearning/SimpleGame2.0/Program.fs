namespace SimpleGame

open System
open System.Threading
open Game
open Brains
open Rendering

module Program =
    // world initialization
    let size = { Width = 40; Height = 20 }
    let player = { Position = { Top = 10; Left = 20 }; Direction = North }
    
    let rng = Random ()
    let board = Array2D.init size.Width size.Height (fun left top ->
        rng.Next(tileValues.Length))

    let score = 0
    let initialGameState = { Board = board; Hero = player; Score = score }

    [<EntryPoint>]
    let main argv =

        let rec loop (state:GameState,brain:Brain) =
            let currentState = visibleState size state.Board state.Hero
            let decision = Brains.decide brain currentState

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

            // world rendering
            let updated = { Board = board; Hero = player; Score = score }
            renderScore score
            render state updated

            Thread.Sleep 200
            loop (updated,brain)

        // start the game
        prepareDisplay size initialGameState
        loop (initialGameState,Map.empty) |> ignore

        0 // return an integer exit code
