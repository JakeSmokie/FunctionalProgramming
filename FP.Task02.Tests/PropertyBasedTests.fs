namespace FP.Task02.Tests

open FsUnit.Xunit
open FP.Task02
open FsCheck
open FsCheck.Xunit

type MyGenerators =
  static member Tree() = {
    new Arbitrary<BinarySearchTree>() with
      override x.Generator =
        let tree' n =
          let nums = Gen.choose (-2_000_000_000, 2_000_000_000) |> Gen.sample 0 n
          Gen.constant (Empty.AddMany nums)

        Gen.sized tree'
      override x.Shrinker t = Seq.empty
  }

[<Properties(Arbitrary=[| typeof<MyGenerators> |])>]
module PropertyBasedTests =
  let rec allElementsEqual f = function
  | [ x ] -> true
  | [] -> true
  | x :: y :: tail when f x y -> allElementsEqual f (y :: tail)
  | _ -> false

  [<Property(MaxTest = 10, StartSize = 1, EndSize = 100)>]
  let ``Concatecation is associative`` (trees : List<BinarySearchTree>) =
    let a = List.fold (fun (acc : BinarySearchTree) x -> acc.Concat x) Empty trees
    let b = List.foldBack (fun (acc : BinarySearchTree) x -> acc.Concat x) trees Empty

    let shuffledSums =
      Gen.shuffle trees
      |> Gen.sample 0 20
      |> List.map (fun trees -> Array.fold (fun (acc : BinarySearchTree) x -> acc.Concat x) Empty trees)

    allElementsEqual ((=)) (a :: b :: shuffledSums)

  [<Property>]
  let ``Zero + A = A + Zero = A`` (tree : BinarySearchTree) =
    let a = Seq.toList ((tree.Concat Empty).Traverse InFix)
    let b = Seq.toList ((Empty.Concat tree).Traverse InFix)
    let c = Seq.toList (tree.Traverse InFix)

    a = c && b = c

  [<Property>]
  let ``InFix Traversal produces sorted list`` (tree : BinarySearchTree) =
    let a = Seq.toList (tree.Traverse InFix)
    let b = Seq.toList (tree.Traverse PostFix)

    a |> should be ascending
    b |> should be descending
    tree.Min |> should equal a.Head
    tree.Max |> should equal b.Head

  [<Property>]
  let ``Trees are balanced and are BST`` (trees : List<BinarySearchTree>) =
    List.forall (fun (t : BinarySearchTree) -> t.IsBalanced && t.IsBST) trees

  [<Property(MaxTest = 1000, StartSize = 10, EndSize = 10000)>]
  let ``List adding works`` (xs : List<int>) =
    let tree = Empty.AddMany xs

    Seq.toList (tree.Traverse InFix)
    |> should equal (List.sort (List.distinct xs))
