#I @"..\..\packages\"
#r @"Deedle.1.2.5\lib\net40\Deedle.dll"

open Deedle

let series1 = series [ "Alpha",1.; "Bravo",2.; "Delta",4. ]
let series2 = series [ "Bravo",20.; "Charlie",30.; "Delta",40. ]

let toyFrame = frame [ "First",series1; "Second",series2 ]

series1 |> Stats.sum
toyFrame |> Stats.mean
toyFrame?Second |> Stats.mean

toyFrame?New <- toyFrame?First + toyFrame?Second
toyFrame |> Stats.mean