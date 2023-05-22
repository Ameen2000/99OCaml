(*Functions to make tree exercises easier*)
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let singleton x =
  Node (x, Leaf, Leaf)

let rec tree_insert x tr =
  match tr with
  | Leaf -> singleton x
  | Node (a, left, right) ->
    match x with
    | x when x > a -> Node (a, left, tree_insert x right)
    | x when x < a -> Node (a, tree_insert x left, right)
    | _ -> Node (x, left, right)

(*Problem 44*)
(*Count the Leaves of the Binary Tree*)
let rec count_leaves tr =
  match tr with
  | Leaf -> 0
  | Node (_, Leaf, Leaf) -> 1
  | Node (_, l, r) -> count_leaves l + count_leaves r

(*Problem 45*)
(*Collect the Leaves of a
 Binary Tree in a list*)
let leaves tr =
  let rec collector tr accum =
    match tr with
    | Leaf -> accum
    | Node (x, Leaf, Leaf) -> x::accum
    | Node (_, l, r) -> (collector l (collector r accum))
  in
  collector tr []

(*Problem 46*)
(*Collect the Internal Nodes of
 a Binary Tree in a list*)
let internals tr =
  let rec collector tr accum =
    match tr with
    | Leaf -> accum
    | Node (x, Leaf, Leaf) -> accum
    | Node (x, l, r) -> (collector l (collector r (x::accum)))
  in
  List.rev @@ collector tr []

(*Problem 47*)
(*Collect the Nodes at a given level*)
let at_level tr i =
  let rec collector tr accum k =
    match tr with
    | Leaf -> accum
    | Node (x, l, r) ->
        if i = k then
          x::accum
        else
          collector l (collector r accum (k+1)) (k+1)
  in
  collector tr [] 1

(*Problem 48*)
(*Construct a complete binary tree*)
let complete_binary_tree lst =
  let rec ctree_insert elem tr =
    match tr with
    | Leaf -> Node (elem, Leaf, Leaf)
    | Node (x, Leaf, Leaf) ->
        Node (x, Node (elem, Leaf, Leaf), Leaf)
    | Node (x, y, Leaf) ->
        Node (x, y, Node (elem, Leaf, Leaf))
    | Node (x, y, z) ->
        Node (x, ctree_insert elem y, z)
  in
  let rec tree_make lst tr_accum =
    match lst with
    | [] -> tr_accum
    | [x] -> ctree_insert x tr_accum
    | [x; y] -> ctree_insert y (ctree_insert x tr_accum)
    | [x; y; z] ->
        ctree_insert z (ctree_insert y (ctree_insert x tr_accum))
    | h::t -> tree_make t (ctree_insert h tr_accum)
  in
  tree_make lst Leaf
