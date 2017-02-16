#I @"..\..\packages\"
#r @"FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"
#r @"MathNet.Numerics.3.17.0\lib\net40\MathNet.Numerics.dll"
#r @"MathNet.Numerics.FSharp.3.17.0\lib\net40\MathNet.Numerics.FSharp.dll"
#load @"FSharp.Charting.0.90.14\FSharp.Charting.fsx"

open FSharp.Charting
open FSharp.Data
open MathNet
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.Providers.LinearAlgebra.Mkl

// mkl
//System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
//Control.LinearAlgebraProvider <- MklLinearAlgebraProvider()

let A = vector [ 1.; 2.; 3. ]
let B = matrix [ [ 1.; 2. ]
                 [ 3.; 4. ]
                 [ 5.; 6. ] ]

let C = A * A
let D = A * B
let E = A * B.Column(1)

[<Literal>]
let Path = "../Data/day.csv"

type Data = CsvProvider<Path>
let dataset = Data.Load(Path)
let data = dataset.Rows

type Vec = Vector<float>
type Mat = Matrix<float>

let cost (theta:Vec) (Y:Vec) (X:Mat) =
    let ps = Y - (theta * X.Transpose())
    ps * ps |> sqrt

let predict (theta:Vec) (v:Vec) = theta * v

let X = matrix [ for obs in data -> [1.; float obs.Instant ] ]
let Y = vector [ for obs in data -> float obs.Cnt ]

let theta = vector [6000.; -4.5]
predict theta (X.Row(0))
cost theta Y X

let estimate (Y:Vec) (X:Mat) =
    (X.Transpose() * X).Inverse() * X.Transpose() * Y

let seed = 314159
let rng = System.Random(seed)
// Fisher-Yates shuffle 
let shuffle arr = 
    let arr = Array.copy arr
    let l = arr.Length
    for i in (l-1) .. -1 .. 1 do
        let temp = arr.[i]
        let j = rng.Next(0, i+1)
        arr.[i] <- arr.[j]
        arr.[j] <- temp
    arr

let training, validation =
    let shuffled = 
        data
        |> Seq.toArray
        |> shuffle
    let size = 0.7 * float (Array.length shuffled) |> int
    shuffled.[..size], shuffled.[size+1..]

type Obs = Data.Row
type Model = Obs -> float
type Featurizer = Obs -> float list

//let exampleFeaturizer (obs:Obs) =
//    [ 1.0; float obs.Instant; ]

let predictor (f:Featurizer) (theta:Vec) =
    f >> vector >> (*) theta

let evaluate (model:Model) (data:Obs seq) =
    data
    |> Seq.averageBy (fun obs -> abs (model obs - float obs.Cnt))

let model (f:Featurizer) (data:Obs seq) =
    let Yt, Xt =
        data
        |> Seq.toList
        |> List.map(fun obs -> float obs.Cnt, f obs)
        |> List.unzip
    let theta = estimate (vector Yt) (matrix Xt)
    let predict = predictor f theta
    theta, predict

let featurizer0 (obs:Obs) =
    [ 1.0; float obs.Instant; ]

let (theta0, model0) = model featurizer0 training

evaluate model0 training |> printfn "Training: %.0f"
evaluate model0 validation |> printfn "Validation: %.0f"

Chart.Combine [
    Chart.Line [ for obs in data -> float obs.Cnt]
    Chart.Line [ for obs in data -> model0 obs ]
]

let featurizer1 (obs:Obs) = 
    [   1.
        float obs.Instant
        float obs.Atemp
        float obs.Hum
        float obs.Temp
        float obs.Windspeed
    ]

let (theta1, model1) = model featurizer1 training
evaluate model1 training |> printfn "Training: %.0f"
evaluate model1 validation |> printfn "Validation: %.0f"

Chart.Combine [
    Chart.Line [ for obs in data -> float obs.Cnt]
    Chart.Line [ for obs in data -> model0 obs ]
    Chart.Line [ for obs in data -> model1 obs ]
]

Chart.Point [ for obs in data -> float obs.Cnt, model1 obs ]
