#I @"..\..\packages\"
#load @"FSharp.Charting.0.90.14\FSharp.Charting.fsx"
#load "KMeans.fs"

open System
open System.IO
open FSharp.Charting
open Unsupervised.KMeans

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

    headers, observations

printfn "%16s %8s %8s %8s" "Tag Name" "Avg" "Min" "Max"
headers
|> Array.iteri (fun i name ->
    let col = observations |> Array.map (fun obs -> obs.[i])
    let avg = col |> Array.average
    let min = col |> Array.min
    let max = col |> Array.max
    printfn "%16s %8.1f %8.1f %8.1f" name avg min max)

let labels = ChartTypes.LabelStyle(Interval=0.25)
headers
|> Seq.mapi (fun i name ->
    name,
    observations
    |> Seq.averageBy (fun obs -> obs.[i]))
|> Chart.Bar
|> fun chart -> chart.WithXAxis(LabelStyle=labels)

type Observation = float[]

let features = headers.Length

let distance (obs1:Observation) (obs2:Observation) =
    (obs1, obs2)
    ||> Seq.map2 (fun u1 u2 -> pown (u1 - u2) 2)
    |> Seq.sum

let centroidOf (cluster:Observation seq) = 
    Array.init features (fun f -> 
        cluster 
        |> Seq.averageBy (fun user -> user.[f]))

let observations1 = 
    observations
    |> Array.map (Array.map float)
    |> Array.filter (fun x -> Array.sum x > 0.)

let clusters1, classifier1 =
    let clustering = clusterize distance centroidOf
    let k = 5
    clustering observations1 k 

clusters1
|> Seq.iter (fun (id, profile) ->
    printfn "CLUSTER %i" id
    profile
    |> Array.iteri (fun i value -> printfn "%16s %.1f" headers.[i] value))

Chart.Combine [
    for (id, profile) in clusters1 ->
        profile
        |> Seq.mapi (fun i value -> headers.[i], value)
        |> Chart.Bar
] 
|> fun chart -> chart.WithXAxis(LabelStyle=labels)

observations1
|> Seq.countBy classifier1
|> Seq.iter (fun (clusterID, count) ->
    printfn "Cluster %i: %i elements" clusterID count)

(*
    Normalized observations
    Row normalization: largest tag = 100% i.e. small distance = users that have same interest profile, not necessarily
    same level of activity on site.
*)
let rowNormalizing (obs:Observation) = 
    let max = obs |> Seq.max
    obs |> Array.map (fun tagUse -> tagUse / max)

let observations2 = 
    observations 
    |> Array.filter (fun x -> x |> Array.sum > 0.)
    // |> Array.map (Array.map float)
    // |> Array.map rowNormalizing
    |> Array.map ((Array.map float) >> rowNormalizing)

let clusters2, classifier2 = 
    let clustering = clusterize distance centroidOf
    let k = 5
    clustering observations2 k

observations2
|> Seq.countBy classifier2
|> Seq.iter (fun (clusterID, count) ->
    printfn "Cluster %i: %i elements" clusterID count)

Chart.Combine [
    for (id, profile) in clusters2 ->
        profile
        |> Seq.mapi (fun i value -> headers.[i], value)
        |> Chart.Column
] 
|> fun chart -> chart.WithXAxis(LabelStyle=labels)

let ruleOfThumb n = sqrt (float n / 2.)
let k_ruleOfThumb = ruleOfThumb (observations2.Length) // approx 25

let squareError (obs1:Observation) (obs2:Observation) = 
    (obs1, obs2)
    ||> Seq.zip
    |> Seq.sumBy (fun (x1, x2) -> pown (x1-x2) 2)

let RSS (dataset:Observation[]) centroids = 
    dataset
    |> Seq.sumBy (fun obs ->
        centroids
        |> Seq.map (squareError obs)
        |> Seq.min)

let AIC (dataset:Observation[]) centroids = 
    let k = centroids |> Seq.length
    let m = dataset.[0] |> Seq.length
    RSS dataset centroids + float (2 * m * k)

[1..25]
|> Seq.map (fun k ->
   let value = 
    [ for _ in 1..10 ->
        let clusters, classifier = 
            let clustering = clusterize distance centroidOf
            clustering observations2 k
        AIC observations2 (clusters |> Seq.map snd) ]
    |> List.average
   k, value)
|> Chart.Line // shows that k 10 produces a minimum

let bestClusters, bestClassifier = 
    let clustering = clusterize distance centroidOf
    let k = 10
    seq {
        for _ in 1..20 ->
            clustering observations2 k
    }
    |> Seq.minBy (fun (cs, _) ->
        RSS observations2 (cs |> Seq.map snd))

bestClusters
|> Seq.iter (fun (id, profile) ->
    printfn "CLUSTER %i" id
    profile
    |> Array.iteri (fun i value -> 
        if value > 0.2 then
            printfn "%16s %.1f" headers.[i] value))
