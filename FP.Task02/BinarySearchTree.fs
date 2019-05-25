namespace FP.Task02

type TraversalType =
  | InFix | PreFix | PostFix

type BinarySearchTree =
  | Empty
  | Node of int * BinarySearchTree * BinarySearchTree

  member this.Add x =
    let rec insert' node =
      match node with
      | Empty -> Node(x, Empty, Empty)
      | Node(value, left, right) when x < value -> Node(value, (insert' left).PerformBalance(), right)
      | Node(value, left, right) when x > value -> Node(value, left, (insert' right).PerformBalance())
      | _ -> node

    (insert' this).PerformBalance()

  member this.AddMany(xs : int seq) =
    Seq.fold (fun (tree : BinarySearchTree) x -> tree.Add x) this xs

  member this.AddMany(xs : int list) =
    List.fold (fun (tree : BinarySearchTree) x -> tree.Add x) this xs

  member this.Delete x =
    let rec findInOrderPredecessor node =
      match node with
      | Empty -> Empty
      | Node(value, _, Empty) -> node
      | Node(_, _, right) -> findInOrderPredecessor right

    let rec delete' x node =
      match node with
      | Empty -> Empty
      | Node(value, left, right) when x < value -> Node(value, (delete' x left).PerformBalance(), right)
      | Node(value, left, right) when x > value -> Node(value, left, (delete' x right).PerformBalance())
      | Node(_, Empty, Empty) -> Empty
      | Node(_, left, Empty) -> left
      | Node(_, Empty, right) -> right
      | Node(_, left, right) ->
        let (Node(value, _, _)) = findInOrderPredecessor left
        Node(value, (delete' value left).PerformBalance(), right)

    (delete' x this).PerformBalance()

  member this.Traverse traversalType =
    let rec traverse' = function
      | Empty -> Seq.empty
      | Node(value, left, right) -> seq {
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

    traverse' this

  member this.Min =
    let rec min' = function
    | Empty -> failwith "Tree is empty"
    | Node(value, Empty, _) -> value
    | Node(_, left, _) -> min' left

    min' this

  member this.Max =
    let rec max' = function
    | Empty -> failwith "Tree is empty"
    | Node(value, _, Empty) -> value
    | Node(_, _, right) -> max' right

    max' this

  member this.Height =
    let rec depth' node d =
      match node with
      | Empty -> d
      | Node(_, left, right) ->
        let l = max d (depth' left (d + 1))
        let r = max d (depth' right (d + 1))

        max l r

    depth' this 0

  member this.Balance =
    match this with
    | Empty -> 0
    | Node(_, left, right) -> left.Height - right.Height

  member this.Deconstruct() =
    match this with
    | Node(x, l, r) -> (x, l, r)
    | Empty -> failwith "Cannot deconstruct empty node"

  member this.PerformBalance() =
    let rotRight() =
      let (x, l, r) = this.Deconstruct()
      let (lx, ll, lr) = l.Deconstruct()

      Node(lx, ll, Node(x, lr, r))

    let rotLeft() =
      let (x, l, r) = this.Deconstruct()
      let (rx, rl, rr) = r.Deconstruct()

      Node(rx, Node(x, l, rl), rr)

    let doubleRotRight() =
      let (x, l, r) = this.Deconstruct()
      let (lx, ll, lr) = l.Deconstruct()
      let (lrx, lrl, lrr) = lr.Deconstruct()

      Node(lrx,
        Node(lx, ll, lrl),
        Node(x, lrr, r)
      )

    let doubleRotLeft() =
      let (x, l, r) = this.Deconstruct()
      let (rx, rl, rr) = r.Deconstruct()
      let (rlx, rll, rlr) = rl.Deconstruct()

      Node(rlx,
        Node(x, l, rll),
        Node(rx, rlr, rr)
      )

    match this with
    | Node(_, l, r) when this.Balance > 1 ->
      if l.Balance >= 1 then rotRight()
      else doubleRotRight()
    | Node(_, l, r) when this.Balance < -1 ->
      if r.Balance <= -1 then rotLeft()
      else doubleRotLeft()
    | _ -> this

  member this.IsBST =
    let rec verify l r node =
      match node with
      | Empty -> true
      | Node(value, left, right) ->
        match l, r with
        | Some left, _ when value < left -> false
        | _, Some right when value > right -> false
        | _ ->
          let l' = defaultArg r value |> min value |> Some
          let r' = defaultArg l value |> max value |> Some

          verify l l' left && verify r' r right

    verify None None this

  member this.IsBalanced =
    let rec verify = function
    | Node(_, l, r) ->
      ((abs this.Balance) <= 1) && verify l && verify r
    | Empty -> true

    verify this

  member this.Concat(other : BinarySearchTree) =
    let a = this.Traverse InFix
    let b = other.Traverse InFix
    let c = Seq.sort (Seq.distinct (Seq.concat [ a; b ]))

    Empty.AddMany c

  member this.Map f =
    Empty.AddMany(Seq.map f (this.Traverse PreFix))

  member this.Filter f =
    let rec filter = function
    | Empty -> Empty
    | Node(x, _, _) as node when f x -> node
    | Node(x, _, _) as node ->
      let (y, l, r) = (node.Delete x).Deconstruct()
      Node(y, filter l, filter r)

    filter this

  member this.Fold f state =
    Seq.fold f state (this.Traverse InFix)

  member this.FoldBack f state =
    Seq.foldBack f (this.Traverse InFix) state
