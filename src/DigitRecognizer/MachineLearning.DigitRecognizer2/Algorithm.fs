namespace MachineLearning.DigitRecognizer2

open System;

module Algorithm =
    // the type that represents each row of the training or the test data
    type Entry = {
        Label: string
        Values: int list }
    
    // calculates squared euclidean distance between pixel values of two images
    let distance values1 values2 =
        values1
        |> List.zip values2
        |> List.sumBy (fun (v1, v2) -> Math.Pow(float(v1) - float(v2), 2.0))
        //|> List.map (fun (v1, v2) -> Math.Pow(float(v1) - float(v2), 2.0))
        //|> List.sum
    
    // a generic k-nearest neighbor algorithm
    let kNN entries newEntry k =
        let newEntryValues = snd newEntry |> Array.toList
        entries
        |> List.map (fun x -> (x.Label, distance x.Values newEntryValues))
        |> List.sortBy snd
        |> List.take k
        |> List.countBy fst
