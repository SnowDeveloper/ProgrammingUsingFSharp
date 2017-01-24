(*

  ITT8060 -- Advanced Programming 2016
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 5: Working with data: intro to type providers and charting

  ------------------------------------
  Name: Madhushree Singh
  TUT Student ID: 166807IVSM
  ------------------------------------


  Answer the questions below.  You answers to questions 1--6 should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the TUT
  git system using the instructions on the course web page by October 29, 2016.
*)

// 1) The following three different fuel consumption units for vehicles are in use:
//    * litres per 100 km
//    * miles per imperial gallon (in use in the UK)
//    * miles per US gallon (in use in the US)
// 1.a) Define the units in terms of units of measure.
// 1.b) Define 2 functions that convert the appropriate US and imperial mpg values to
//      litres per 100 km. 
// 1.c) Define a function that converts litres per 100 km of appropriate fuel to
//      CO2 emissions g per km.
[<Measure>] type km
[<Measure>] type ml
[<Measure>] type litres
[<Measure>] type usGallons
[<Measure>] type iGallons
[<Measure>] type hundKm
// Define the conversion references
let mileToHundkm = 0.01609<hundKm/ml>
let usGalToLitres = 3.78541<litres/usGallons>
let iGalToLitres = 4.5460<litres/iGallons>

// Converts miles per gallon to litres per 100km
let mpgtoLhkm (mpg: float<ml/usGallons>) = usGalToLitres / (mileToHundkm * mpg)
mpgtoLhkm 45.0<ml/usGallons> 

// Convert miles per imperial gallon to litres per 100km
let impgToLhkm (mpg: float<ml/iGallons>) = iGalToLitres / (mileToHundkm * mpg)
impgToLhkm 88.75<ml/iGallons> 

// Gram measure
[<Measure>] type gram
// Define the conversion references
let hkmToKm = 100.0<hundKm/km>
let litresToGram = 4.3103<litres/gram>

// Convert the litres per 100km to gram per km
let lhkmToGkm (litresKm: float<litres/hundKm>) = litresKm * hkmToKm / litresToGram
lhkmToGkm 45.25<litres/hundKm>

// 2) Get the fuel consumption data
// 2.a) in imperial MPG (miles per imperial gallon) of at least 20 vehicles from
// http://carfueldata.direct.gov.uk/search-by-fuel-economy.aspx
// Save the data in file called imperial.csv

// 2.b) Get the fuel consumption data of at least 20 cars in US MPG (miles per US gallon) from
// https://www.fueleconomy.gov/feg/download.shtml
// save the data in file called us.csv

// 3) Load the imperial.csv and us.csv files using FSharp.Data.CsvProvider<>
#r @"packages\FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"

open System
open FSharp.Data

type usProvider = CsvProvider<"us.csv">
let usPData = usProvider.Load(__SOURCE_DIRECTORY__ + "/us.csv")

type providerImp = CsvProvider<"imperial.csv">
let iPData = providerImp.Load(__SOURCE_DIRECTORY__ + "/imperial.csv")
   

// 4) Write a function to convert the appropriate mpg data into
//    litres per 100 km using the functions defined in Q1.
let convertUSData (data: Runtime.CsvFile<CsvRow>) = 
    data.Rows |> Seq.map (fun (row: CsvRow) -> mpgtoLhkm((row.GetColumn "Comb Unrd Adj FE - Conventional Fuel").AsFloat() * 1.0<ml/usGallons>))

let convertImpData (data: Runtime.CsvFile<CsvRow>) = 
    data.Rows |> Seq.map (fun (row: CsvRow) -> impgToLhkm((row.GetColumn "Imperial Combined").AsFloat() * 1.0<ml/iGallons>))
    
let convertData (csvData: Runtime.CsvFile<CsvRow>) (flag: string) =
    match flag with
    | "u" -> convertUSData csvData
    | _ -> convertImpData csvData
        



// 5) Display the converted data in an appropriate chart (select the type that is most 
//    appropriate for displaying the data).
#r @"packages\FSharp.Charting.0.90.14\lib\net40\FSharp.Charting.dll"
#load @"packages\FSharp.Charting.0.90.14\FSharp.Charting.fsx"
open FSharp.Charting
open FSharp.Charting.ChartTypes
open FSharp.Data.CsvExtensions

let usCsvData = CsvFile.Load(__SOURCE_DIRECTORY__ + "/us.csv").Cache()
let impCsvData = CsvFile.Load(__SOURCE_DIRECTORY__ + "/imperial.csv").Cache()

let impDataChart = [for row in impCsvData.Rows -> (row.GetColumn "Model"), impgToLhkm((row.GetColumn "Imperial Combined").AsFloat() * 1.0<ml/iGallons>)]
                 |> Chart.Column |> Chart.WithXAxis (LabelStyle = ChartTypes.LabelStyle(Angle = -45, Interval = 1.0)) |> Chart.WithTitle "Imperial"
    
let usDataChart = [for row in usCsvData.Rows -> (row.GetColumn "Carline"), mpgtoLhkm((row.GetColumn "Comb Unrd Adj FE - Conventional Fuel").AsFloat() * 1.0<ml/usGallons>)]
                |> Chart.Column |> Chart.WithXAxis (LabelStyle = ChartTypes.LabelStyle(Angle = -45, Interval = 1.0)) |> Chart.WithTitle "US"

// 6) Combine the data from 2 data sources into a single chart. Add appropriate titles and
//    legends. 
Chart.Combine (
    [ usDataChart
      impDataChart]) |> Chart.WithTitle "Combined"

