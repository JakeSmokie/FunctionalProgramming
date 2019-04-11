module FP.Task02.Tests.UnitTests

open Xunit
open FsUnit.Xunit
open FP.Task02
open FP.Task02.Tests

let zero = BinarySearchTree<int>.Zero
let zeroNode = BSTNode<int>.Empty

let bigTreeElements = [3; 1; 8; 0; 4; 9; 10; 5; 2; 100; 50; 75; 80; 51; 99; -100; -200; -150; 10000]
let treeAsArray = [3; 1; 0; -100; -200; -150; 2; 8; 4; 5; 9; 10; 100; 50; 75; 51; 80; 99; 10000]
let unbalancedTreeElements = [10000101 .. 10000200] @ [10000001 .. 10000100]

let listToTree = List.fold ((+)) zero

[<Fact>]
let ``Root element is not mutable`` () =
  let mutable root = zero.Root
  root <- (Node(5, Node(6, Empty, Empty), Empty))

  zero.Root |> should equal zeroNode
  BSTTester.testNode root |> should be False

[<Fact>]
let ``Insert method is pure`` () =
  zero.Insert 4 |> ignore
  zero.Root |> should equal zeroNode

[<Fact>]
let ``Insertion works`` () =
  let changed = zero + 3 + 5 + 1 + 2 + 4
  
  changed.Root |> should equal (
    Node(3, 
      Node(1, 
        Empty, 
        Node(2, Empty, Empty)
      ), 
      Node(5, 
        Node(4, Empty, Empty), 
        Empty
      )
    )
  )

  BSTTester.testNode changed.Root |> should be True

[<Fact>]
let ``Deletion works`` () =
  let changed = zero + 3 - 3 + 3 + 5 + 1 + 2 + 4 - 3 + 6 + 7 + 0

  changed.Root |> should equal (
    Node(2,
      Node(1, 
        Node(0, Empty, Empty), 
        Empty
      ), 
      Node(5, 
        Node(4, Empty, Empty), 
        Node(6, 
          Empty,
          Node(7, Empty, Empty)
        )
      )
    )
  )

  (changed - 5).Root |> should equal (
    Node(2,
      Node(1, 
        Node(0, Empty, Empty), 
        Empty
      ), 
      Node(4, 
        Empty, 
        Node(6, 
          Empty,
          Node(7, Empty, Empty)
        )
      )
    )
  )

[<Fact>]
let ``Min and Max methods do work correctly`` () =
  let bigTree = bigTreeElements |> listToTree

  bigTree.Min |> should equal -200
  bigTree.Max |> should equal 10000

[<Fact>]
let ``Depth method works correctly`` () = 
  let unbalancedTree = unbalancedTreeElements |> listToTree
  unbalancedTree.Depth |> should equal 101
  
  zero.Depth |> should equal 0
  (zero + 1).Depth |> should equal 1
  (zero + 1 + 2).Depth |> should equal 2
  (zero + 1 + 2 + 0).Depth |> should equal 2

[<Fact>]
let ``IEnumerable traversal operators works correctly`` () =
  let bigTree = bigTreeElements |> listToTree

  let inFix = bigTree >>= InFix |> Seq.toList
  let postFix = bigTree >>= PostFix |> Seq.toList
  let preFix = bigTree >>= PreFix |> Seq.toList

  inFix |> should equal (List.sort bigTreeElements)
  postFix |> should equal (List.sortDescending bigTreeElements)
  preFix |> should equal treeAsArray

[<Fact>]
let ``BST concatecation works correctly`` () = 
  let unbalancedTree = unbalancedTreeElements |> listToTree
  let bigTree = bigTreeElements |> listToTree
  let mergedTree = unbalancedTree + bigTree

  Seq.length mergedTree
  |> should equal (Seq.length unbalancedTree + Seq.length bigTree)

  mergedTree
  |> Seq.toList
  |> should equal (unbalancedTreeElements @ bigTreeElements |> List.sort)
