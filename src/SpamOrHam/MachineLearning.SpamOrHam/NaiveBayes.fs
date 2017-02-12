namespace NaiveBayes

module Classifier = 
    type Token = string
    type TokenizedDoc = Token Set
    type Tokenizer = string -> TokenizedDoc

    type DocsGroup = {
        Proportion:float;
        TokenFrequencies:Map<Token,float>;
    }

    let tokenScore (group:DocsGroup) (token:Token) =
        match group.TokenFrequencies.TryFind token with
        | Some f -> log f
        | None -> 0.0
//        if group.TokenFrequencies.ContainsKey token then
//            log group.TokenFrequencies.[token]
//        else
//            0.0

    (*
        We do this because instead of the raw probability we want to compute a "logarithmic score". 
        This is for ease of calculation (addition instead of multiplication) and to avoid underflows as probabilities can be small.
    *)
    let score (document:TokenizedDoc) (group:DocsGroup) =
        let scoreToken = tokenScore group
        log group.Proportion + (document |> Seq.sumBy scoreToken)

    let classify (groups:(_*DocsGroup)[]) (tokenizer:Tokenizer) (txt:string) =
        let tokenized = tokenizer txt
        groups
        |> Array.maxBy(fun (label, group) -> score tokenized group)
        |> fst

    let proportion count total = float count / float total
    let laplace count total = float (count + 1) / float (total + 1)

    let countIn (group:TokenizedDoc seq) (token:Token) =
        group
        |> Seq.filter (Set.contains token)
        |> Seq.length

    let analyze (group:TokenizedDoc seq) (totalDocs:int) (classificationTokens:Token Set) =
        let groupSize = group |> Seq.length
        let score token =
            let count = countIn group token
            laplace count groupSize
        let scoredTokens = 
            classificationTokens
            |> Set.map(fun token -> token, score token)
            |> Map.ofSeq
        let groupProportion = proportion groupSize totalDocs
        
        {
            Proportion = groupProportion
            TokenFrequencies = scoredTokens
        }

    let learn (docs:(_*string)[]) (tokenizer:Tokenizer) (classificationTokens:Token Set) =
        let total = docs.Length
        docs
        |> Seq.map(fun (label, doc) -> label, tokenizer doc)
        |> Seq.groupBy fst
        |> Seq.map(fun (label, group) -> label, group |> Seq.map snd)
        |> Seq.map(fun (label, group) -> label, analyze group total classificationTokens)
        |> Seq.toArray

    let train (docs:(_*string)[]) (tokenizer:Tokenizer) (classificationTokens:Token Set) =
        let groups = learn docs tokenizer classificationTokens
        let classifier = classify groups tokenizer
        classifier
