#I @"..\..\packages\"
#r @"FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"

open FSharp.Data

type Questions = JsonProvider<"""https://api.stackexchange.com/2.2/questions?site=stackoverflow""">

//let csQuestions = """https://api.stackexchange.com/2.2/questions?site=stackoverflow&tagged=C%23"""
//Questions.Load(csQuestions).Items |> Seq.iter(fun q -> printfn "%s" q.Title)

let questionQuery = """https://api.stackexchange.com/2.2/questions?site=stackoverflow"""
let tagged tags query =
   let joinedTags = tags |> String.concat ";"
   sprintf "%s&tagged=%s" query joinedTags
let page p query = sprintf "%s&page=%i" query p
let pageSize s query = sprintf "%s&pagesize=%i" query s
let extractQuestions (query:string) = Questions.Load(query).Items

let ``C#`` = "C%23"
let ``F#`` = "F%23"

let fsSample = 
    questionQuery
    |> tagged [``F#``]
    |> pageSize 100
    |> extractQuestions

let csSample = 
    questionQuery
    |> tagged [``C#``]
    |> pageSize 100
    |> extractQuestions

let analyzeTags (qs:Questions.Item seq) =
    qs
    |> Seq.collect(fun q -> q.Tags)
    |> Seq.countBy id
    |> Seq.filter(fun (_, count) -> count > 2)
    |> Seq.sortByDescending(fun (_, count) -> count)
    |> Seq.iter(fun (tag, count) -> printfn "%s,%i" tag count)

analyzeTags fsSample
analyzeTags csSample
