open Expecto
open FP.Task02.BinarySearchTree
open FP.Task02.BinarySearchTree
open FSharpPlus
open FsCheck

type BSTGen() =
  static member BST() : int BST Arbitrary =
    Gen.listOf Arb.generate
    |> Gen.map ofList
    |> Arb.fromGen

let config = {
  FsCheckConfig.defaultConfig with
    maxTest = 1000
    endSize = 1000
    arbitrary = [
      typeof<BSTGen>
    ]
 }

let fastConfig = {
  config with
    maxTest = 50
    endSize = 100
}

[<Tests>]
let tests = testList "BST" [
  testList "Generic operations" [
    testPropertyWithConfig config "Generated tree is BST" <|
      fun (a : int BST) ->
        isBST a
    
    testPropertyWithConfig config "BST produces sorted list of ints" <|
      fun (xs : int list) ->
        (xs |> BST<int>.OfList |> toList) = (xs |> List.distinct |> List.sort)
  
    testPropertyWithConfig config "BST deletion works" <|
      fun (xs : int list) (ys : int list) ->
        (ys
         |> fold (fun acc x -> delete x acc) (xs |> BST<int>.OfList)
         |> toList) = (xs |> List.except ys |> List.distinct |> List.sort)
    
    testPropertyWithConfig config "BST is balanced" <|
      fun (a : int BST) ->
        balanceFactor a |> abs <= 1
  ]

  testList "Monoid operations" [
    testPropertyWithConfig fastConfig "Monoid addition is associative" <|
      fun (a : int BST) (b : int BST) (c : int BST) ->
        (a ++ b) ++ c = a ++ (b ++ c)
    
    testPropertyWithConfig fastConfig "Monoid addition with zero gives same BST" <|
      fun (a : int BST) ->
        let b = a ++ Empty
        let c = Empty ++ a
        
        a = b && a = c
  ]
 ]

[<EntryPoint>]
let main args =
  runTestsInAssemblyWithCLIArgs [] args
