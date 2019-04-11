module FP.Task02.Tests.PropertyBasedTests

open FP.Task02
open FsUnit.Xunit
open FsCheck.Xunit

[<Property>]
let ``Min and Max methods work correctly`` (xs : List<int>) =
  let elems = (xs @ [1; 2; 3]) |> List.distinct
  let tree = elems |> List.fold ((+)) BinarySearchTree.Zero  

  tree.Min |> should equal (List.min elems)
  tree.Max |> should equal (List.max elems)

[<Property>]
let ``Traversals work correctly`` (xs : List<int>) =
  let elems = (xs @ [1; 2; 3]) |> List.distinct
  let tree = elems |> List.fold ((+)) BinarySearchTree.Zero  

  tree >>= InFix 
  |> Seq.toList
  |> should equal (List.sort elems)
  
  tree >>= PostFix 
  |> Seq.toList
  |> should equal (List.sortDescending elems)
