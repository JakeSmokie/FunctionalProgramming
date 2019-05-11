module FP.Task03.CsvPointsReader

let parseLine (line : string) =
  let coords =
    line.Split(';') |> Array.map float

  (coords.[0], coords.[1])

let readPoints() =
  Seq.initInfinite (fun _ -> stdin.ReadLine())
  |> Seq.takeWhile (fun s -> s <> null && s <> "")
  |> Seq.map parseLine
  |> Seq.cache

let printPoints points =
  Seq.iter (fun (x, y) -> printfn "%f;%f" x y) points
