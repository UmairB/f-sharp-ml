namespace MachineLearning.DigitRecognizer2

open System;
open System.IO;
open MachineLearning.DigitRecognizer2.Algorithm;

module Loader =
    let loadValues filename =
        File.ReadAllLines filename
        |> Seq.ofArray
        |> Seq.skip 1
        |> Seq.map (fun line ->
            {
                Label = line.Substring(0, line.IndexOf(','))
                Values = line.Split(',')
                    |> Seq.ofArray
                    |> Seq.skip 1 // first token is the label so skip it
                    |> Seq.map Convert.ToInt32
                    |> Seq.toList
            })
        |> Seq.toList
    