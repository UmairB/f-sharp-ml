open System.IO

type Observation = {
    Label: string
    Pixels: int[]
}

let toObservation (csvData:string) = 
    let columns = csvData.Split(',')
    let label = columns.[0]
    let pixels = columns.[1..] |> Array.map int
    {
        Label = label
        Pixels = pixels
    }

// distance related
type Distance = int[] -> int[] -> int

let manhattenDistance pixelsOne pixelsTwo =
   Array.zip pixelsOne pixelsTwo
   |> Array.map(fun (x, y) -> abs(x - y))
   |> Array.sum 

let euclideanDistance pixelsOne pixelsTwo =
   Array.zip pixelsOne pixelsTwo 
   |> Array.map(fun (x, y) -> pown (x - y) 2)
   |> Array.sum

let reader path =
    let data = File.ReadAllLines path
    data.[1..] |> Array.map toObservation

let train (trainingSet:Observation[]) (distance:Distance) =
    let classify (pixels) =
        trainingSet
        |> Array.minBy(fun x -> distance x.Pixels pixels)
        |> fun x -> x.Label
    classify

let evaluate data classifier =
    data
    |> Array.averageBy(fun x -> if classifier x.Pixels = x.Label then 1. else 0.)

let trainingDataPath = Path.Combine(__SOURCE_DIRECTORY__, "../Data/trainingsample.csv")
let training = reader trainingDataPath

let manhattenClassifier = train training manhattenDistance
let euclideanClassifier = train training euclideanDistance

let validationDataPath = Path.Combine(__SOURCE_DIRECTORY__, "../Data/validationsample.csv")
let validation  = reader validationDataPath

printfn "Manhatten: Correct: %.3f" (evaluate validation manhattenClassifier)
printfn "Euclidean: Correct: %.3f" (evaluate validation euclideanClassifier)
