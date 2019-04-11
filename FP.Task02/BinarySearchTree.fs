namespace FP.Task02

open System.Collections.Generic

type BSTNode<'a> = 
  | Empty
  | Node of value: 'a * left: BSTNode<'a> * right: BSTNode<'a>

type TraversalType = 
  | InFix
  | PreFix
  | PostFix

type BinarySearchTree<'a when 'a : comparison> private (treeRoot) =
  let root = treeRoot  

  interface IEnumerable<'a> with
    member this.GetEnumerator(): IEnumerator<'a> = 
      (this.Traverse InFix).GetEnumerator()

    member this.GetEnumerator(): System.Collections.IEnumerator = 
       (this.Traverse InFix).GetEnumerator() :> System.Collections.IEnumerator
  
  member this.Insert x =
    let rec insert' node = 
      match node with
      | Empty -> Node (x, Empty, Empty)
      | Node (value, left, right) when x < value -> Node (value, insert' left, right)
      | Node (value, left, right) when x > value -> Node (value, left, insert' right)
      | _ -> sprintf "%A already exists" x |> failwith 

    BinarySearchTree(insert' root)

  member this.Delete x =
    let rec findInOrderPredecessor node =
      match node with
      | Empty -> Empty
      | Node (_, _, Empty) -> node
      | Node (_, _, right) -> findInOrderPredecessor right 

    let rec delete' x node =
      match node with
      | Empty -> Empty
      | Node (value, left, right) when x < value -> Node (value, delete' x left, right)
      | Node (value, left, right) when x > value -> Node (value, left, delete' x right)
      | Node (_, Empty, Empty) -> Empty
      | Node (_, left, Empty) -> left
      | Node (_, Empty, right) -> right
      | Node (_, left, right) ->
        let (Node(value, _, _)) = findInOrderPredecessor left
        Node (value, delete' value left, right)

    BinarySearchTree(delete' x root)

  member this.Traverse traversalType =
    let rec traverse' = function
      | Empty -> Seq.empty
      | Node (value, left, right) -> seq {
        match traversalType with
        | PreFix ->
          yield value
          yield! traverse' left
          yield! traverse' right
        | InFix ->
          yield! traverse' left
          yield value
          yield! traverse' right
        | PostFix ->
          yield! traverse' right
          yield value
          yield! traverse' left
      }
      
    traverse' root

  member this.Min 
    with get() = 
      let rec min' = function
      | Empty -> failwith "Tree is empty"
      | Node (value, Empty, _) -> value
      | Node (_, left, _) -> min' left

      min' root

  member this.Max
    with get() = 
      let rec max' = function
      | Empty -> failwith "Tree is empty"
      | Node (value, _, Empty) -> value
      | Node (_, _, right) -> max' right

      max' root

  member this.Depth
    with get() = 
      let rec depth' node d =
        match node with
        | Empty -> d
        | Node (_, left, right) -> 
          let l = max d (depth' left (d + 1))
          let r = max d (depth' right (d + 1))

          max l r

      depth' root 0

  member this.Root
    with get() =
      root

  static member (+) (a: BinarySearchTree<'a>, b: 'a) = a.Insert b
  static member (-) (a: BinarySearchTree<'a>, b: 'a) = a.Delete b
  static member (>>=) (a: BinarySearchTree<'a>, b: TraversalType) = a.Traverse b
  static member Zero with get() = BinarySearchTree<'a>()

  member this.Concatecate otherTree =
    let insert' = Seq.fold (fun (tree: BinarySearchTree<'a>) x -> tree.Insert x)
    
    this >>= PreFix
    |> insert' (BinarySearchTree<'a>()) >>= PreFix
    |> insert' otherTree
  
  static member (+) (a: BinarySearchTree<'a>, b: BinarySearchTree<'a>) = a.Concatecate b
  private new() = BinarySearchTree<'a>(Empty)