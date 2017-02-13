#I @"..\..\packages\"
#r @"FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"
#r @"R.NET.Community.1.6.5\lib\net40\RDotNet.dll"
#r @"RProvider.1.1.20\lib\net40\RProvider.Runtime.dll"
#r @"RProvider.1.1.20\lib\net40\RProvider.dll"
#r @"Deedle.1.2.5\lib\net40\Deedle.dll"
#r @"Deedle.RPlugin.1.2.5\lib\net40\Deedle.RProvider.Plugin.dll"

open FSharp.Data
open Deedle
open Deedle.RPlugin
open RProvider
open RProvider.``base``
open RProvider.rworldmap

let wb = WorldBankData.GetDataContext()
let countries = wb.Countries

let population2000 = series [ for c in countries -> c.Code, c.Indicators.``Population, total``.[2000] ]
let population2010 = series [ for c in countries -> c.Code, c.Indicators.``Population, total``.[2010] ]
let surface = series [ for c in countries -> c.Code, c.Indicators.``Surface area (sq. km)``.[2010] ]

let ddf = 
    frame [
        "Pop2000", population2000
        "Pop2010", population2010
        "Surface", surface
    ]

ddf?Code <- ddf.RowKeys

let map = R.joinCountryData2Map(ddf, "ISO3", "Code")
//R.mapCountryData(map, "Pop2000")

ddf?Density <- ddf?Pop2010 / ddf?Surface
//R.mapCountryData(map, "Density")

ddf?Growth <- (ddf?Pop2010 - ddf?Pop2000) / ddf?Pop2000
R.mapCountryData(map, "Growth")
