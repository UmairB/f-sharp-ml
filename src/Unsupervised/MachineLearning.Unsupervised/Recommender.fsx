open System
open System.IO

let folder = Path.Combine(__SOURCE_DIRECTORY__, "..", "Data")
let file = "userprofiles-toptags.txt"

let headers, observations = 
    let raw = 
        Path.Combine(folder, file) 
        |> File.ReadAllLines

    // first column is headers, first col is UuerID
    let headers = (raw.[0].Split(',')).[1..]
    let observations = 
        raw.[1..]
        |> Array.map ((fun line -> (line.Split ',').[1..]) >> (Array.map float))
        |> Array.filter (fun x -> Array.sum x > 0.)

    headers, observations

let scale (row:float[]) =
    let min = row |> Array.min
    let max = row |> Array.max
    if min = max then row
    else row |> Array.map (fun x -> (x - min) / (max - min))

let test = observations.[..99] |> Array.map scale
let train = observations.[100..] |> Array.map scale

let distance (row1:float[]) (row2:float[]) =
    (row1, row2)
    ||> Array.map2 (fun x y -> pown (x - y) 2)
    |> Array.sum

let similarity (row1:float[]) (row2:float[]) =
    1. / (1. + distance row1 row2)

let split (row:float[]) =
    row.[..19], row.[20..]

let weights (values:float[]) =
    let total = values |> Array.sum
    values
    |> Array.map (fun x -> x / total)

let predict (row:float[]) =
    let known, unknown = row |> split
    let similarities =
        train
        |> Array.map (fun example ->
            let common, _ = example |> split
            similarity known common)
        |> weights
    [|  
        for i in 20 .. 29 ->
            let column = train |> Array.map (fun x -> x.[i])
            let prediction =
                (similarities, column)
                ||> Array.map2 (*)//(fun s v -> s * v)
                |> Array.sum
            prediction
    |]

let targetTags = headers.[20..]
predict test.[0] |> Array.zip targetTags
test.[0] |> split |> snd // actual

let validation =
    test
    |> Array.map (fun obs ->
        let actual = obs |> split |> snd
        let predicted = obs |> predict
        let recommended, observed = 
            Array.zip predicted actual
            |> Array.maxBy fst
        
        if observed > 0. then 1. else 0.)
    |> Array.average
    |> printfn "Correct calls: %f"

// naive recommendation
let baseline = 
    let averages = [|
        for i in 20 .. 29 ->
            train |> Array.averageBy (fun row -> row.[i]) |]

    test
    |> Array.map (fun obs ->
        let actual = obs |> split |> snd
        let predicted = averages
        let recommended, observed = 
            Array.zip predicted actual
            |> Array.maxBy fst
        
        if observed > 0. then 1. else 0.)
    |> Array.average
    |> printfn "Correct calls: %f"
