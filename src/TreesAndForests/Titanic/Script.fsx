#I @"..\..\packages\"
#r @"FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"

open FSharp.Data

[<Literal>]
let path = "../Data/titanic.csv"

type Titanic = CsvProvider<path>
type Passenger = Titanic.Row

let dataset = Titanic.GetSample()

dataset.Rows
|> Seq.countBy (fun passenger -> passenger.Survived)
|> Seq.iter (printfn "%A")

dataset.Rows
|> Seq.averageBy (fun passenger ->
    if passenger.Survived then 1.0 else 0.0)
|> printfn "Chances of survival: %.3f"

let survivalRate (passengers:Passenger seq) =
    let total = passengers |> Seq.length
    let survivors = 
        passengers
        |> Seq.filter (fun p -> p.Survived)
        |> Seq.length
    100.0 * (float survivors / float total)

let survivedByFeature grouping print =
    let grouping = 
        dataset.Rows
        |> Seq.groupBy grouping

    grouping
    |> Seq.iter (fun (s, g) ->
        printfn "%s %A: %f" print s (survivalRate g))

survivedByFeature (fun p -> p.Sex) "Sex"
survivedByFeature (fun p -> p.Pclass) "Class"

let mostFrequentLabelIn group =
    group
    |> Seq.countBy snd
    |> Seq.maxBy snd
    |> fst

let learn sample extractFeature extractLabel =
    // group together observations that have the
    // same value for the selected feature, and
    // find the most frequent label by group.
    let groups =
        sample
        |> Seq.map (fun obs -> extractFeature obs, extractLabel obs)
        |> Seq.groupBy fst
        |> Seq.map (fun (feat, group) -> feat, mostFrequentLabelIn group)
    // for an observation, find the group with
    // matching feature value, and predict the
    // most frequent label for that group.
    let classifier obs =
        let featureValue = extractFeature obs
        groups
        |> Seq.find (fun (f, _) -> f = featureValue)
        |> snd
    classifier

let survived (p:Passenger) = p.Survived

let sexClassifier = survived |> learn (dataset.Rows) (fun p -> p.Sex)

printfn "Stump: classify based on passenger sex."
dataset.Rows
|> Seq.averageBy (fun p ->
    if p.Survived = sexClassifier p then 1.0 else 0.0)
|> printfn "Survived: %.3f"

let classClassifier = survived |> learn (dataset.Rows) (fun p -> p.Pclass)

printfn "Stump: classify based on passenger class."
dataset.Rows
|> Seq.averageBy (fun p ->
    if p.Survived = classClassifier p then 1.0 else 0.0)
|> printfn "Survived: %.3f"

// using numbers for grouping by feature
let survivalByPricePaid =
    dataset.Rows
    |> Seq.groupBy (fun p -> p.Fare)
    |> Seq.iter (fun (price, passengers) ->
        printfn "%6.2F: %6.2f" price (survivalRate passengers))

let fareLevel =
    let averageFare = 
        dataset.Rows
        |> Seq.averageBy (fun p -> p.Fare) 
 
    fun (p:Passenger) ->
        if p.Fare < averageFare 
        then "Cheap"
        else "Expensive" 

let fareClassifier = survived |> learn (dataset.Rows) fareLevel

printfn "Stump: classify based on fare level"
dataset.Rows
|> Seq.averageBy (fun p ->
    if p.Survived = fareClassifier p then 1.0 else 0.0)
|> printfn "Survived: %.3f"

// missing data
let survivalByPortOfOrigin =
    dataset.Rows
    |> Seq.groupBy (fun p -> p.Embarked)
    |> Seq.iter (fun (port, passengers) ->
        printfn "%s: %f" port (survivalRate passengers))

let hasData extractFeature = extractFeature >> Option.isSome

let betterLearn sample extractFeature extractLabel =
    let branches =
        sample
        |> Seq.filter (extractFeature |> hasData)
        |> Seq.map (fun obs -> extractFeature obs |> Option.get, extractLabel obs)
        |> Seq.groupBy fst
        |> Seq.map (fun (feat, group) -> feat, mostFrequentLabelIn group)
        |> Map.ofSeq
    let labelForMissingValues =
        sample
        |> Seq.countBy extractLabel
        |> Seq.maxBy snd
        |> fst
    let classifier obs =
        match (extractFeature obs) with 
        | None -> labelForMissingValues
        | Some(value) ->
            match (branches.TryFind value) with
            | None -> labelForMissingValues
            | Some(predictedLabel) -> predictedLabel

    classifier

let port (p:Passenger) =
    if p.Embarked = "" then None
    else Some(p.Embarked)

let updatedClassifier = survived |> betterLearn (dataset.Rows) port
