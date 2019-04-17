module FP.Task01.ResultToPairsParsingMethod
open FSharpPlus
open FSharp.Collections.ParallelSeq

let makePalindrome x =
  let s = string x
  int (s + rev s)

let palindromes =
  Seq.initInfinite makePalindrome

let getResult n =
  let maxPalindrome = pown 10 (n * 2)
  let maxMultiplier = pown 10 n

  let rec checkFactors a b =
    match a, b with
    | _, _ when b < maxMultiplier / 10 -> false
    | _, _ when a % b = 0 && a / b < maxMultiplier -> true
    | _, _ -> checkFactors a (b - 1)

  let p = PSeq.takeWhile (fun x -> x < maxPalindrome) palindromes
  let filtered = PSeq.filter (fun x -> checkFactors x (maxMultiplier - 1)) p
  let max = PSeq.max filtered

  max
