namespace SimpleGame

open System
open Game

module Brains =

    // my current direction and surrounding cells
    type State = Dir * (Cell option) list

    type Experience = {
        State: State; // where I was
        Action: Act; // what I did
        Reward: float; // what reward I got
        NextState: State; } // where I ended
    
    // one possible course of action I can take
    // when I am in that State
    type Strategy = { State:State; Action:Act; }

    // Strategies I tried, and what reward
    // I should expect from them
    type Brain = Map<Strategy,float>

    let rng = Random ()
    let choices = [| Straight; Left; Right; |]
    let randomDecide () = choices.[rng.Next(3)]

    let private alpha = 0.2 // learning rate
    let learn (brain:Brain) (exp:Experience) =
        let strat = { State = exp.State; Action = exp.Action }
        match brain.TryFind strat with
        | Some(value) ->
            brain.Add (strat, (1.0-alpha) * value + alpha * exp.Reward)
        | None -> brain.Add (strat, (alpha * exp.Reward))

    let decide (brain:Brain) (state:State) =
        let knownStrategies =
            choices
            |> Array.map (fun alt -> { State = state; Action = alt })
            |> Array.filter brain.ContainsKey

        match knownStrategies.Length with
        | 0 -> randomDecide ()
        | _ ->
            choices
            |> Seq.maxBy (fun alt ->
                let strat = { State = state; Action = alt }
                match brain.TryFind strat with
                | Some(value) -> value
                | None -> 0.0)

    let tileAt (board:Board) (pos:Pos) = board.TryFind pos

    let offsets = 
        [ (-1,1)
          (-1,0)
          (-1,1)
          (0,-1)
          (0,1)
          (1,-1)
          (1,0)
          (1,1) ]
    
    let visibleState (size:Size) (board:Board) (hero:Hero) =
        let (dir,pos) = hero.Direction, hero.Position
        let visibleCells =
            offsets
            |> List.map (fun (x,y) ->
                onboard size { Top = pos.Top + x; Left = pos.Left + y }
                |> tileAt board)
        (dir,visibleCells)
