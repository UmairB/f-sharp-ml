#load "NaiveBayes.fs"

open System.IO
open System.Text.RegularExpressions
open NaiveBayes.Classifier

type DocType = 
    | Ham
    | Spam
    
let parseDocType label =
    match label with
    | "ham" -> Ham
    | "spam" -> Spam
    | _ -> failwith (sprintf "Unknown label: %A" label)

let parseLine (line:string) =
    let split = line.Split('\t')
    let label = split.[0] |> parseDocType
    let message = split.[1]
    (label, message)

let matchWords = Regex(@"\w+")
let wordTokenizer (text:string) =
    text.ToLowerInvariant()
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map(fun m -> m.Value)
    |> Set.ofSeq
let casedTokenizer (text:string) =
    text
    |> matchWords.Matches
    |> Seq.cast<Match>
    |> Seq.map(fun m -> m.Value)
    |> Set.ofSeq

let phone = 
    let phoneWords = Regex(@"0[7-9]\d{0}")
    let phone (text:string) =
        match (phoneWords.IsMatch text) with
        | true -> "__PHONE__"
        | false -> text
    phone
let txt = 
    let txtCode = Regex(@"\b\d{5}\b")
    let txt (text:string) =
        match (txtCode.IsMatch text) with
        | true -> "__TXT__"
        | false -> text
    txt

let vocabulary (tokenizer:Tokenizer) (corpus:string seq) =
    corpus
    |> Seq.map tokenizer
    |> Set.unionMany

let top n (tokenizer:Tokenizer) (docs:string[]) =
    let tokenized = docs |> Array.map tokenizer
    let tokens = tokenized |> Set.unionMany
    tokens 
    |> Seq.sortBy(fun t -> -countIn tokenized t)
    |> Seq.take n
    |> Set.ofSeq

let rareTokens n (tokenizer:Tokenizer) (docs:string[]) =
    let tokenized = docs |> Array.map tokenizer
    let tokens = tokenized |> Set.unionMany
    tokens 
    |> Seq.sortBy(fun t -> countIn tokenized t)
    |> Seq.take n
    |> Set.ofSeq

let filename = "SMSSpamCollection"
let path = Path.Combine(__SOURCE_DIRECTORY__, "..", "Data", filename)

let dataset = 
    File.ReadAllLines path
    |> Array.map parseLine

let validation = dataset |> Array.take 1000
let training = dataset |> Array.skip 1000

let evaluate (tokenizer:Tokenizer) (tokens: Token Set) (log:string) =
    let classifier = train training tokenizer tokens
        
    validation
    |> Seq.averageBy(fun(docType, sms) ->
        if docType = classifier sms then 1.0 else 0.0)
    |> printfn "%s, correctly classified: %.3f" log

let allTokens = 
    training
    |> Seq.map snd
    |> vocabulary wordTokenizer
let casedTokens = 
    training
    |> Seq.map snd
    |> vocabulary casedTokenizer

evaluate wordTokenizer (["txt"] |> set) "Based on 'txt'"
evaluate wordTokenizer allTokens "Based on all tokens"
evaluate casedTokenizer casedTokens "Based on cased tokenizer and all tokens"

let ham, spam =
    let rawHam, rawSpam =
        training
        |> Array.partition(fun (label, _) -> label = Ham)
    rawHam |> Array.map snd, 
    rawSpam |> Array.map snd

let hamCount = ham |> vocabulary casedTokenizer |> Set.count
let spamCount = spam |> vocabulary casedTokenizer |> Set.count

let topHam = ham |> top (hamCount / 10) casedTokenizer
let topSpam = spam |> top (spamCount / 10) casedTokenizer

let topTokens = Set.union topHam topSpam

evaluate casedTokenizer topTokens "Based on top tokens"

let commonTokens = Set.intersect topHam topSpam
let specificTokens = Set.difference topTokens commonTokens

evaluate casedTokenizer specificTokens "Based on top specific tokens"

//ham |> rareTokens 50 casedTokenizer |> Seq.iter (printfn "%s");;
//spam |> rareTokens 50 casedTokenizer |> Seq.iter (printfn "%s");;

let smartTokenizer = casedTokenizer >> Set.map phone >> Set.map txt
let smartTokens = 
    specificTokens
    |> Set.add "__TXT__"
    |> Set.add "__PHONE__"

evaluate smartTokenizer smartTokens "Based on smart tokenizer and tokens"

let lengthAnalysis len =
    let long (msg:string) = msg.Length > len

    let ham, spam =
        dataset 
        |> Array.partition(fun (docType, _) -> docType = Ham)
    let spamAndLongCount =
        spam 
        |> Array.filter(fun(_, sms) -> long sms)
        |> Array.length
    let longCount =
        dataset
        |> Array.filter(fun(_, sms) -> long sms)
        |> Array.length

    let pSpam = (float spam.Length) / (float dataset.Length)
    let pLongIfSpam = (float spamAndLongCount) / (float spam.Length)
    let pLong = (float longCount) / (float dataset.Length)

    let pSpamIfLong = pLongIfSpam * pSpam / pLong
    pSpamIfLong

let bestClassifier = train training smartTokenizer smartTokens
validation
|> Seq.filter(fun (docType, _) -> docType = Ham)
|> Seq.averageBy(fun (docType, sms) -> if docType = bestClassifier sms then 1. else 0.)
|> printfn "Properly classified Ham: %.5f"
validation
|> Seq.filter(fun (docType, _) -> docType = Spam)
|> Seq.averageBy(fun (docType, sms) -> if docType = bestClassifier sms then 1. else 0.)
|> printfn "Properly classified Spam: %.5f"


