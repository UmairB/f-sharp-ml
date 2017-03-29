﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.IO
open MachineLearning.DigitRecognizer2.Algorithm
open MachineLearning.DigitRecognizer2.Loader
open MachineLearning.DigitRecognizer2.Render

let getDigitRecognizerFolder () =
    let dirInfo = DirectoryInfo(Directory.GetCurrentDirectory())
    let rec getDataDir (dir:DirectoryInfo) =
        if dir |> isNull
        then None
        else if dir.Name = "DigitRecognizer" 
        then Some dir.FullName
        else getDataDir dir.Parent
    getDataDir dirInfo

[<EntryPoint>]
let main argv =
    let path = 
        match getDigitRecognizerFolder () with
        | None -> ""
        | Some d -> Path.Combine(d, "Data", "trainingsample.csv")

    let loaded = loadValues path

    //Here is the unknown entry 
    let newEntry = ("X",[|0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;25;123;245;243;211;45;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;71;227;252;166;47;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;20;197;249;223;47;2;68;232;98;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;48;202;252;155;35;0;15;225;252;80;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;190;252;155;7;0;0;110;252;208;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;123;253;109;0;0;0;68;245;216;18;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;253;224;14;0;0;15;211;252;153;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;54;253;71;0;0;9;237;252;224;7;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;106;253;111;37;91;204;253;252;126;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;18;253;252;235;252;208;253;252;82;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;62;106;106;35;0;255;204;9;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;80;253;89;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;158;253;63;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;43;239;225;21;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;64;252;167;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;169;253;45;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;8;197;252;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;22;252;244;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;22;252;173;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;13;217;121;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|])

    let pixels = snd newEntry |> Array.map float
    // only consider 5 nearest neighbors
    let k = 5
    // getting back the labels for each of the nearest neighbours 
    let labels = kNN loaded newEntry k

    //Locating the guess. The one with the maximum votes 
    let guess = labels |> List.item 0 |> fst
    //Answer will be 9
    drawDigit pixels ("I think that it is a " + guess)
    0 // return an integer exit code
