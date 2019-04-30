module FP.Task03.CsvPointsReader
open System
open System
open FSharp.Data

type Points =
  CsvProvider<Separators=";",
              Schema="x (float), y (float)",
              HasHeaders=false>

let rec readlines() = seq {
  let line = stdin.ReadLine()
  if line <> null then
    yield Points.ParseRows(line).[0]
    yield! readlines()
}

let readPoints() =
  readlines() |> Seq.map (fun row -> (row.X, row.Y))

let printPoints points =
  Seq.iter (fun (x, y) -> printfn "%f;%f" x y) points
