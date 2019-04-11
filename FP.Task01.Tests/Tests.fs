module Tests

open Xunit
open FsUnit.Xunit
open FP.Task01
open CS.Task01

let palindromes = [
  9009L
  906609L
  99000099L
  9966006699L
  999000000999L
  99956644665999L
]

let checkMethod f n = 
  List.map f [2 .. 1 + n]
  |> should equal (List.take n palindromes)

[<Fact>]
let ``Loops method produces correct palindrome values`` () =
  checkMethod (LoopsMethod.getResult >> int64) 2

[<Fact>]
let ``ResultToPairsParsingMethod produces correct palindrome values`` () =
  checkMethod (ResultToPairsParsingMethod.getResult >> int64) 3

[<Fact>]
let ``PairsToResultMethod produces correct palindrome values`` () =
  checkMethod (PairsToResultMethod.getResult >> int64) 3

[<Fact>]
let ``SharpMethod produces correct palindrome values`` () =
  checkMethod SharpMethod.GetResult 5
