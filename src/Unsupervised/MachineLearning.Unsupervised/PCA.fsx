#I @"..\..\packages\"
#r @"MathNet.Numerics.3.17.0\lib\net40\MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.3.17.0\lib\net40\MathNet.Numerics.FSharp.dll"
#load @"FSharp.Charting.0.90.14\FSharp.Charting.fsx"
#load @"PCA.fs"

open System
open System.IO
open MathNet
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open FSharp.Charting
open Unsupervised.PCA

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

let correlations =
    observations
    |> Matrix.Build.DenseOfColumnArrays
    |> Matrix.toRowArrays
    |> Correlation.PearsonMatrix

let feats = headers.Length
let correlated = 
    seq {
        for col in 0..(feats-1) do
            for row in (col+1)..(feats-1) do
                yield correlations.[col,row], headers.[col], headers.[row]
    }
    |> Seq.sortByDescending (fun (corr, _, _) -> abs corr)
    |> Seq.take 20
    |> Seq.iter (fun (corr, f1, f2) ->
        printfn "%s %s : %.2f" f1 f2 corr)

let normalized = normalize (headers.Length) observations
let (eValues, eVectors), projector = pca normalized

let total = eValues |> Seq.sumBy (fun x -> x.Magnitude)
eValues
|> Vector.toList
|> List.rev // math.NET returns eigenvalues and eigenvectors in ascending eigenvalue order. Customary to look at features in descending order
|> List.scan (fun (percent, cumul) value ->
    let percent = 100. * value.Magnitude / total
    let cumul = cumul + percent
    (percent, cumul)) (0., 0.)
|> List.tail
|> List.iteri (fun i (p, c) -> printfn "Feat %2i: %.2f%% (%.2f%%)" i p c)

let principalComponent comp1 comp2 =
    let title = sprintf "Component %i vs %i" comp1 comp2
    let features = headers.Length
    let coords = Seq.zip (eVectors.Column(features - comp1)) (eVectors.Column(features-comp2))
    Chart.Point (coords, Title = title, Labels = headers, MarkerSize = 7)
    |> Chart.WithXAxis(Min = -1.0, Max = 1.0,
        MajorGrid = ChartTypes.Grid(Interval = 0.25),
        LabelStyle = ChartTypes.LabelStyle(Interval = 0.25),
        MajorTickMark = ChartTypes.TickMark(Enabled = false))
    |> Chart.WithYAxis(Min = -1.0, Max = 1.0,
        MajorGrid = ChartTypes.Grid(Interval = 0.25),
        LabelStyle = ChartTypes.LabelStyle(Interval = 0.25),
        MajorTickMark = ChartTypes.TickMark(Enabled = false))

let projections comp1 comp2 =
    let title = sprintf "Component %i vs %i" comp1 comp2
    let features = headers.Length
    let coords = 
        normalized
        |> Seq.map projector
        |> Seq.map (fun obs -> obs.[features - comp1], obs.[features-comp2])
    Chart.Point (coords, Title = title)
    |> Chart.WithXAxis(Min = -200.0, Max = 500.0,
        MajorGrid = ChartTypes.Grid(Interval = 100.),
        LabelStyle = ChartTypes.LabelStyle(Interval = 100.),
        MajorTickMark = ChartTypes.TickMark(Enabled = false))
    |> Chart.WithYAxis(Min = -200.0, Max = 500.0,
        MajorGrid = ChartTypes.Grid(Interval = 100.),
        LabelStyle = ChartTypes.LabelStyle(Interval = 100.),
        MajorTickMark = ChartTypes.TickMark(Enabled = false))
