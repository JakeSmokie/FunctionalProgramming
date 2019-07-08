module FP.Task02.BinarySearchTree
open FSharpPlus

type TraversalType =
  | InFix | PreFix | PostFix

type 'a BST =
  | Empty
  | Node of 'a * 'a BST * 'a BST

let depth node =
  let rec depth' d = function
    | Empty -> d
    | Node(x, l, r) ->
      let l = depth' (d + 1) l
      let r = depth' (d + 1) r
      
      max l r

  depth' 0 node

let balanceFactor = function
  | Empty -> 0
  | Node(_, l, r) -> depth l - depth r

let balance node =
  let rotRight node =
    let (Node(x, l, r)) = node
    let (Node(lx, ll, lr)) = l

    Node(lx, ll, Node(x, lr, r))

  let rotLeft node =
    let (Node(x, l, r)) = node
    let (Node(rx, rl, rr)) = r

    Node(rx, Node(x, l, rl), rr)

  let doubleRotRight node =
    let (Node(x, l, r)) = node
    let (Node(lx, ll, lr)) = l
    let (Node(lrx, lrl, lrr)) = lr

    Node(lrx,
      Node(lx, ll, lrl),
      Node(x, lrr, r)
    )

  let doubleRotLeft node =
    let (Node(x, l, r)) = node
    let (Node(rx, rl, rr)) = r
    let (Node(rlx, rll, rlr)) = rl

    Node(rlx,
      Node(x, l, rll),
      Node(rx, rlr, rr)
    )
  
  match node with
  | Node(_, l, r) when balanceFactor node > 1 ->
    if balanceFactor l >= 1 then rotRight node
    else doubleRotRight node
  | Node(_, l, r) when balanceFactor node < -1 ->
    if balanceFactor r <= -1 then rotLeft node
    else doubleRotLeft node
  | _ -> node

let rec insert x node =
  match node with
  | Empty -> Node(x, Empty, Empty)
  | Node(value, left, right) when x < value -> Node(value, insert x left |> balance, right) |> balance
  | Node(value, left, right) when x > value -> Node(value, left, insert x right |> balance) |> balance
  | _ -> node
  
let delete x node =
  let rec findInOrderPredecessor node =
    match node with
    | Empty -> Empty
    | Node(value, _, Empty) -> node
    | Node(_, _, right) -> findInOrderPredecessor right

  let rec delete' x node =
    match node with
    | Empty -> Empty
    | Node(value, left, right) when x < value -> Node(value, delete' x left |> balance, right)
    | Node(value, left, right) when x > value -> Node(value, left, delete' x right |> balance)
    | Node(_, Empty, Empty) -> Empty
    | Node(_, left, Empty) -> left
    | Node(_, Empty, right) -> right
    | Node(_, left, right) ->
      let (Node(value, _, _)) = findInOrderPredecessor left
      Node(value, delete' value left |> balance, right)

  delete' x node
    
type 'a BST with
  static member ToSeq x =
    match x with
    | Empty -> Seq.empty
    | Node(x, l, r) -> seq {
      yield! toSeq l
      yield x
      yield! toSeq r
    }
  
  static member ToList (x : _ BST) =
    x |> toSeq |> toList

  static member OfSeq (x : _ seq) =
    fold (fun acc x -> insert x acc) Empty x
  
  static member OfList (x : _ list) =
    fold (fun acc x -> insert x acc) Empty x
  
  static member Map(x : 'a BST, f: 'a -> 'b) : 'b BST =
    toSeq x |>> f |> ofSeq
  
  static member Zero =
    Empty
  
  static member Return x =
    Node(x, Empty, Empty)
  
  static member (+) (a : _ BST, b : _ BST) : _ BST =
    match a, b with
    | Empty, _ -> b
    | _, Empty -> a
    | _, _ -> 
      [a; b] >>= toSeq
      |> Seq.sort
      |> ofSeq

let isBST (node : BST<_>) =
  node
  |> toSeq
  |> Seq.pairwise
  |> Seq.forall (fun (a, b) -> a < b)
  