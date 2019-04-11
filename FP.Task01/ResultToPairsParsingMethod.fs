module FP.Task01.ResultToPairsParsingMethod
open FSharpPlus
open FSharp.Collections.ParallelSeq

let makePalindrome x =
  let s = x |> string
  s + rev s |> int

let palindromes = 
  Seq.initInfinite (fun x -> makePalindrome x)

let getResult n =
  let maxPalindrome = int (10.0 ** (float n * 2.0))
  let maxMultiplier = int (10.0 ** float n)

  let rec checkFactors a b =
    match a, b with
    | _, _ when b < maxMultiplier / 10 -> false
    | _, _ when a % b = 0 && a / b < maxMultiplier -> true
    | _, _ -> checkFactors a (b - 1)

  let p = PSeq.takeWhile (fun x -> x < maxPalindrome) palindromes
  let filtered = PSeq.filter (fun x -> checkFactors x (maxMultiplier - 1)) p
  let max = PSeq.max filtered
  
  max