module NaiveBayesTests

open FsUnit
open NUnit.Framework
open NaiveBayes.Classifier

[<TestFixture>]
type TokenScoreShould() =
    [<Test>]
    member this.CalculateCorrectScore() = 
        let group = {
            Proportion = 1.0
            TokenFrequencies = [("a", 2.4); ("b", 7.5); ("c", 4.9)] |> Map.ofList
        }
        let token = "a"
        
        let expected = log group.TokenFrequencies.[token]

        tokenScore group token |> should equal expected

    [<Test>]
    member this.Return0ForMissingToken() =
        let group = {
            Proportion = 1.0
            TokenFrequencies = [("a", 2.4); ("b", 7.5); ("c", 4.9)] |> Map.ofList
        }
        let token = "f"

        tokenScore group token |> should equal 0.0

[<TestFixture>]
type ScoreShould() =
    [<Test>]
    member this.CalculateCorrectScore() = 
        let group = {
            Proportion = 0.5
            TokenFrequencies = [("a", 2.4); ("b", 7.5); ("c", 4.9)] |> Map.ofList
        }
        let document = ["a"; "c"; "d"] |> Set.ofList
        
        let scoreToken = tokenScore group
        let expected = log group.Proportion + (document |> Seq.sumBy scoreToken)

        score document group |> should equal expected

[<TestFixture>]
type ClassifyShould() =
    [<Test>]
    member this.CorrectlyClassifyHamMessage() =
        let txt = "Hello how are you?"
        let tokenizer (str:string) = 
            str.Split(' ') |> Set.ofArray
        let groups = 
            [|
                (
                    "spam", 
                    { 
                        Proportion = 0.2 
                        TokenFrequencies = [("free", 2.4); ("money", 7.5); ("for", 7.5); ("you", 4.9)] |> Map.ofList
                    }
                )
                (
                    "ham",
                    { 
                        Proportion = 0.8
                        TokenFrequencies = [("a", 2.4); ("b", 7.5); ("c", 4.9)] |> Map.ofList
                    }
                )
            |]
        
        classify groups tokenizer txt |> should equal "ham"
