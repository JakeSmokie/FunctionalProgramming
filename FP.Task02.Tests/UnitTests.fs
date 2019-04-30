module FP.Task02.Tests.UnitTests

open System
open Xunit
open FsUnit.Xunit
open FP.Task02

let zero = Empty

let bigTreeElements = [ 5; 3; 7; 1; 4; 6; 10 ]
let treeAsArray = [ 5; 3; 1; 4; 7; 6; 10 ]
let unbalancedTreeElements = [ 1..500 ]

[<Fact>]
let ``Root element is not mutable``() =
  let mutable root = zero
  root <- (Node(5, Node(6, Empty, Empty), Empty))

  zero |> should equal Empty
  root.IsBST |> should be False

[<Fact>]
let ``Add method is pure``() =
  zero.Add 4 |> ignore
  zero |> should equal Empty

[<Fact>]
let ``Insertion works``() =
  let a = Empty.AddMany [ 3; 5; 1; 2; 4 ]

  a.IsBST |> should be True
  a |> should equal (
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

[<Fact>]
let ``Deletion works``() =
 (Empty.AddMany bigTreeElements).Delete 3
  |> should equal (Empty.AddMany [ 5; 7; 1; 4; 6; 10 ])

 (Empty.AddMany bigTreeElements).Delete 5
  |> should equal (Empty.AddMany [ 4; 7; 3; 1; 6; 10 ])

 (Empty.AddMany bigTreeElements).Delete 1
  |> should equal (Empty.AddMany [ 5; 7; 3; 4; 6; 10 ])

 (Empty.AddMany bigTreeElements).Delete 7
  |> should equal (Empty.AddMany [ 5; 3; 6; 1; 4; 10 ])


[<Fact>]
let ``Min and Max methods do work correctly``() =
  let bigTree = Empty.AddMany bigTreeElements

  bigTree.Min |> should equal 1
  bigTree.Max |> should equal 10

[<Fact>]
let ``Height method works correctly``() =
  zero.Height |> should equal 0
  (zero.Add 1).Height |> should equal 1
  (zero.AddMany [ 1; 2 ]).Height |> should equal 2
  (zero.AddMany [ 1; 2; 0 ]).Height |> should equal 2

[<Fact>]
let ``IEnumerable traversal operators works correctly``() =
  let bigTree = Empty.AddMany bigTreeElements

  let inFix = Seq.toList (bigTree.Traverse InFix)
  let postFix = Seq.toList (bigTree.Traverse PostFix)
  let preFix = Seq.toList (bigTree.Traverse PreFix)

  inFix |> should equal (List.sort bigTreeElements)
  postFix |> should equal (List.sortDescending bigTreeElements)
  preFix |> should equal treeAsArray

[<Fact>]
let ``Monoid operations work``() =
  let first = Empty.AddMany [ 2; 1; 3 ]
  let second = Empty.AddMany [ -2; -1; -3; 0 ]
  let third = [ -3; -2; -1; 0; 1; 2; 3 ]

  Empty.Concat Empty |> should equal Empty
  Empty.Concat first |> should equal first
  first.Concat Empty |> should equal first
  Seq.toList ((first.Concat second).Traverse InFix) |> should equal third
  Seq.toList ((second.Concat first).Traverse InFix) |> should equal third

  let a = Empty.AddMany [ 1..100 ]
  let b = Empty.AddMany [ 101..200 ]

  (a.Concat b) |> should equal (Empty.AddMany [ 1..200 ])
  (b.Concat a) |> should equal (Empty.AddMany [ 1..200 ])
  (a.Concat a) |> should equal a

[<Fact>]
let ``Left rotation works``() =
  let tree = Empty.AddMany [ 1; 2; 3 ]
  let balanced = Empty.AddMany [ 2; 1; 3 ]

  tree.IsBST |> should be True
  tree |> should equal balanced

[<Fact>]
let ``Right rotation works``() =
  let tree = Empty.AddMany [ 3; 2; 1 ]
  let balanced = Empty.AddMany [ 2; 1; 3 ]

  tree.IsBST |> should be True
  tree |> should equal balanced

[<Fact>]
let ``Double Left rotation works``() =
  let tree = Empty.AddMany [ 10; 1; 20; 17; 40; 15 ]
  let balanced = Empty.AddMany [ 17; 10; 20; 1; 15; 40 ]

  tree.IsBST |> should be True
  tree |> should equal balanced

[<Fact>]
let ``Double Right rotation works``() =
  let tree = Empty.AddMany [ 10; 5; 20; 1; 7; 9 ]
  let balanced = Empty.AddMany [ 7; 5; 10; 1; 9; 20 ]

  tree.IsBST |> should be True
  tree |> should equal balanced

[<Fact>]
let ``Unbalanced tree is marked as unbalanced``() =
  let ubTree = Node(0, Empty, Node(1, Empty, Node(2, Empty, Node(3, Empty, Empty))))
  ubTree.IsBalanced |> should be False

  let bTree = Node(0, Node(-1, Empty, Empty), Node(1, Empty, Node(2, Empty, Empty)))
  bTree.IsBalanced |> should be True

[<Fact>]
let ``Unbalanced input gets balanced``() =
  let tree = Empty.AddMany unbalancedTreeElements

  tree.IsBST |> should be True
  tree.IsBalanced |> should be True
  tree.Height |> should lessThanOrEqualTo (
    int (log (float unbalancedTreeElements.Length) / log 2.0) + 1
  )

[<Fact>]
let ``Is special case covered``() =
  let a = Empty.AddMany [ -3; -2; -1; ]
  a |> should equal (
    Node(-2,
      Node(-3, Empty, Empty),
      Node(-1, Empty, Empty)
    )
  )

  let b = a.Add 0
  b |> should equal (
    Node(-2,
      Node(-3, Empty, Empty),
      Node(-1,
        Empty,
        Node(0, Empty, Empty)
      )
    )
  )

  let c = b.Add 1
  c |> should equal (
    Node(-2,
      Node(-3, Empty, Empty),
      Node(0,
        Node(-1, Empty, Empty),
        Node(1, Empty, Empty)
      )
    )
  )

  let d = c.Add 2
  d |> should equal (
    Node(0,
      Node(-2,
        Node(-3, Empty, Empty),
        Node(-1, Empty, Empty)
      ),
      Node(1,
        Empty,
        Node(2, Empty, Empty)
      )
    )
  )

[<Fact>]
let ``Adding same element doesn't affect tree``() =
  let tree = Empty.AddMany [ 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; ]

  tree |> should equal (
    Node(1, Node(0, Empty, Empty), Empty)
  )

[<Fact>]
let ``Specific case with double left rotation``() =
  let tree = Empty.AddMany [3; 1; 2; -2; -1; 0]

  Seq.toList (tree.Traverse InFix)
    |> should equal [-2; -1; 0; 1; 2; 3]
