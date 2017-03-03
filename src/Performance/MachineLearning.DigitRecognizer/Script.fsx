open System
open System.IO

type Observation = { Label:string; Pixels:int[] }
type Distance = int[] * int[] -> int
type Classifier = int[] -> string

let toObservation (csvData:string) = 
    let columns = csvData.Split(',')
    let label = columns.[0]
    let pixels = columns.[1..] |> Array.map int
    { Label = label; Pixels = pixels }

let reader path =
    let data = File.ReadAllLines path
    data.[1..] 
    |> Array.map toObservation

let trainingPath = Path.Combine(__SOURCE_DIRECTORY__, "../../DigitRecognizer/Data/trainingsample.csv")
let training = reader trainingPath

let euclideanDistance (pixelsOne, pixelsTwo) =
   Array.zip pixelsOne pixelsTwo 
   |> Array.map(fun (x, y) -> pown (x-y) 2)
   |> Array.sum

let d2 (pixels1, pixels2) =
   (pixels1, pixels2) // using map2 instead of zip + map
   ||> Array.map2(fun x y -> (x-y) * (x-y)) // remove pown
   |> Array.sum

let d3 (pixels1:int[], pixels2:int[]) = // remove piping and use recursion
    let dim = pixels1.Length
    let rec f acc i =
        if i = dim
        then acc
        else
            let x = pixels1.[i] - pixels2.[i]
            let acc' = acc + (x * x)
            f acc' (i + 1)
    f 0 0

let d4 (pixels1:int[], pixels2:int[]) = // use imperative approach + mutation
    let dim = pixels1.Length
    let mutable dist = 0
    for i in 0 .. (dim - 1) do
        let x = pixels1.[i] - pixels2.[i]
        dist <- dist + (x * x)
    dist

let train (trainingSet:Observation[]) (distance:Distance) =
    let classify pixels =
        trainingSet
        |> Array.minBy(fun x -> distance (x.Pixels, pixels))
        |> fun x -> x.Label
    classify

let validationPath = Path.Combine(__SOURCE_DIRECTORY__, "../../DigitRecognizer/Data/validationsample.csv")
let validation  = reader validationPath

let evaluate data classifier =
    data
    |> Array.averageBy(fun x -> if classifier x.Pixels = x.Label then 1. else 0.)
    |> printfn "Correct: %.3f"

let euclideanModel = train training euclideanDistance

let img1 = training.[0].Pixels
let img2 = training.[1].Pixels

#time "on"

for i in 1 .. 1000000 do
    let dist = d4 (img1, img2)
    ignore ()

#time "off"
