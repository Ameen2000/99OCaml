(*Functions to make tree exercises easier*)
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let singleton x =
  Node (x, Leaf, Leaf)

let tree_head tr =
  match tr with
  | Leaf -> raise (Invalid_argument "It's empty")
  | Node(x, _, _) -> x

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
