module FP.Task03.CsvPointsReader
open System.IO
open FSharp.Data

type Points =
  CsvProvider<Separators=";",
              Schema="x (float), y (float)",
              HasHeaders=false>

let getPointsOfCsv (file : string) =
  let points = Points.Load(file)

  points.Rows
  |> Seq.map (fun row -> (row.X, row.Y))
  |> Seq.toList

let savePointsToCsv (points : list<float * float>) (file : string) =
  if File.Exists(file) then File.Delete (file)
  
  let doc = new Points(List.map (Points.Row) points)
  doc.Save(file)
