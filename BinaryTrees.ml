(*Functions to make tree exercises easier*)
exception Empty

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let number_nodes tr =
  let rec aux tr accum =
    match tr with
    | Leaf -> accum
    | Node (root, left, right) -> aux left (aux right (accum + 1))
  in
  aux tr 0

let singleton x =
  Node (x, Leaf, Leaf)

let tree_head tr =
  match tr with
  | Leaf -> None
  | Node(x, _, _) -> Some x

let rec tree_insert x tr =
  match tr with
  | Leaf -> singleton x
  | Node (a, left, right) ->
    match x with
    | x when x > a -> Node (a, left, tree_insert x right)
    | x when x < a -> Node (a, tree_insert x left, right)
    | _ -> Node (x, left, right)

(*Problem 44*)
(*Not solved yet*)
let rec left_insert x tr =
  match tr with
  | Leaf -> raise Empty
  | Node (root, Leaf, right) -> Node (root, Node(x, Leaf, Leaf), right)
  | Node (root, left, right) -> Node (root, left_insert x left, right)

let rec right_insert x tr =
  match tr with
  | Leaf -> raise Empty
  | Node (root, left, Leaf) -> Node (root, left, Node (x, Leaf, Leaf))
  | Node (root, left, right) -> Node (root, left, right_insert x right)

(*Problem 45*)
(*Not solved yet*)
let construct lst =
  match lst with
  | [] -> Leaf
  | _ -> List.fold_right tree_insert (List.rev lst) Leaf

(*Problem 46*)
(*Not solved yet*)

(*Problem 47*)
(*Not solved yet*)

(*Problem 48*)
(*Not solved yet*)

(*Problem 49*)
(*Not solved yet*)

(*Problem 50*)
(*Count the Leaves of the Binary Tree*)
let rec count_leaves tr =
  match tr with
  | Leaf -> 0
  | Node (_, Leaf, Leaf) -> 1
  | Node (_, l, r) -> count_leaves l + count_leaves r

(*Problem 51*)
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

(*Problem 52*)
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

(*Problem 53*)
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

(*Problem 54*)
(*Construct a complete binary tree*)
let rec split_at lst n acc =
  match (n , lst) with
  | (0, _) -> (List.rev acc, lst)
  | (_, []) -> (List.rev acc, [])
  | (_, h::t) -> split_at t (n-1) (h::acc)

let rec tree_make vals trees =
  match (vals, trees) with
  | (vals, []) ->
      List.map (fun x -> Node(x, Leaf, Leaf)) vals
  | (h::t, [x]) -> Node(h, x, Leaf)::(tree_make t [])
  | (h1::t1, h2::m2::t2) -> Node(h1, h2, m2)::(tree_make t1 t2)
  | _ -> raise (Invalid_argument "lol")

let complete_binary_tree lst =
  match lst with
  | [] -> Leaf
  | h::t ->
      let rec aux n lst =
        match lst with
        | [] -> []
        | lst ->
            let vals, trees = split_at lst (1 lsl n) [] in
            tree_make vals (aux (n+1) trees)
      in
      List.hd (aux 0 lst)
