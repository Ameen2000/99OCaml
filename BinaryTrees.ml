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
let add_trees_with left_trees right_trees all =
  let add_right_tree all l =
    List.fold_left (fun a r -> Node ('x', l, r) :: a) all right_trees in
  List.fold_left add_right_tree all left_trees

let rec cbal_tree n = 
  if n = 0 then [Leaf]
  else if n mod 2 = 1
  then
    let tr = cbal_tree (n / 2) in
    add_trees_with tr tr []
  else
    let tr1 = cbal_tree (n / 2 - 1) in
    let tr2 = cbal_tree (n / 2) in
    add_trees_with tr1 tr2 (add_trees_with tr2 tr1 [])

(*Problem 45*)
let rec is_mirror tr1 tr2 =
  match tr1, tr2 with
  | Leaf, Leaf -> true
  | Node (_, l1, r1), Node (_, l2, r2) ->
      is_mirror l1 r2 && is_mirror l2 r1
  | _ -> false

let is_symmetric tr =
  match tr with
  | Leaf -> true
  | Node (_, left, right) ->
      is_mirror left right

(*Problem 46*)
let construct lst =
  match lst with
  | [] -> Leaf
  | _ -> List.fold_right tree_insert (List.rev lst) Leaf

(*Problem 47*)
(* Construct all symmetric, completely balanced binary trees*)
let sym_cbal_trees n =
  List.filter is_symmetric (cbal_tree n)

(*Problem 48*)
let rec hbal_tree n =
  if n = 0 then [Leaf]
  else if n = 1 then [Node ('x', Leaf, Leaf)]
  else
    let t1 = hbal_tree (n - 1) in
    let t2 = hbal_tree (n - 2) in
    add_trees_with t1 t1 @@ 
    add_trees_with t1 t2 @@ 
    add_trees_with t2 t1 []

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
