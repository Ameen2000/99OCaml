(*Problem 40*)
(*Truth Tables for Logical Expressions (2 Variables)*)
type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr*bool_expr
  | Or of bool_expr*bool_expr

exception LogicalError of string

let rec eval var1 val1 var2 val2 expr =
  match expr with
  | Var x ->
    if x = var1 then val1
    else if x = var2 then val2
    else raise (LogicalError "Variable not in expression")
  | Not e -> not (eval var1 val1 var2 val2 expr)
  | And (e1, e2) -> (eval var1 val1 var2 val2 e1) && (eval var1 val1 var2 val2 e2)
  | Or (e1, e2) -> (eval var1 val1 var2 val2 e1) || (eval var1 val1 var2 val2 e2)

let table2 var1 var2 expr =
  [(true, true, eval var1 true var2 true expr);
   (true, false, eval var1 true var2 false expr);
    (false, true, eval var1 false var2 true expr);
  (false, false, eval var1 false var2 false expr)]

(*Problem 41*)
(*Truth tables continued*)
let rec eval_map vals expr =
  match expr with
  | Var x -> List.assoc x vals
  | Not e -> not (eval_map vals e)
  | And (e1, e2) -> (eval_map vals e1) && (eval_map vals e2)
  | Or (e1, e2) -> (eval_map vals e1) || (eval_map vals e2)

let table vars expr =
  let rec table_inner vals vars expr =
  match vars with
  | [] -> [(List.rev vals, eval_map vals expr)]
  | h::t ->
      table_inner ((h, true)::vals) t expr
    @ table_inner ((h, false)::vals) t expr
  in
  table_inner [] vars expr

(*Problem 42*)
(*Gray Codes*)
let gray n =
  let start = ["0"; "1"] in
  let rec gray_inner n accum =
    let join x y = x^y in
    let l1 = List.map (join "0") accum in
    let l2 = List.rev @@ List.map (join "1") accum in
    match n with
    | 1 -> accum
    | n -> gray_inner (n-1) (l1@l2)
  in
  gray_inner n start

(*Problem 43*)
(*Huffman Codes*)
(*No solution yet*)
module Huffman =
  struct
    type huffman_tree =
      | Leaf of string * int
      | Node of int * huffman_tree * huffman_tree

    let tuple_to_leaf (ch1, n1) =
      Leaf (ch1, n1)

    let node x y = 
      match (x, y) with
      | (Leaf (_, n1), Leaf (_, n2)) -> Node (n1+n2, x, y)
      | (Leaf (ch1, n1), Node (k, left, right)) ->
          Node (k + n1, Leaf (ch1, n1), Node (k, left, right))
      | (Node (k, left, right), Leaf (ch1, n1)) ->
          Node (k + n1, Node (k, left, right), Leaf (ch1, n1))
      | (Node (k1, left1, right1), Node (k2, left2, right2)) ->
          Node (k1 + k2, Node (k1, left1, right1), Node (k2, left2, right2))

    let sort lst =
      let comparer x y =
        match (x, y) with
        | (Leaf (_, n1), Leaf (_, n2)) -> compare n1 n2
        | (Node (k, _, _), Leaf (_, n1)) -> compare k n1
        | (Leaf (_, n1), Node (k, _, _)) -> compare k n1
        | (Node (k1, _, _), Node (k2, _, _)) -> compare k1 k2
    in
    List.sort comparer lst

    let prio htree =
      match htree with
      | Leaf (_, priority) -> priority
      | Node (priority, _, _) -> priority

    let rec insert helem hlst =
      match hlst with
      | [] -> [helem]
      | h::t ->
          if prio h < prio helem
          then h :: (insert helem t)
          else helem :: hlst

    let huffman_make lst =
      let hlst = List.map tuple_to_leaf lst in
      let sorted = sort hlst in
      let rec aux lst =
        match lst with
        | [elem] -> elem
        | [x; y] -> node x y
        | h::m::t -> aux (insert (node h m) t)
        | _ -> assert false
      in
      aux sorted
  end

module type Huffman =
  sig
    type huffman_tree
    val tuple_to_leaf : string * int -> huffman_tree
    val node : huffman_tree -> huffman_tree -> huffman_tree
    val sort : huffman_tree list -> huffman_tree list
    val prio : huffman_tree -> int
    val insert : huffman_tree -> huffman_tree list
    val huffman_make : (string * int) list -> huffman_tree
  end

let binary_traverse elem tr =
  let open Huffman in
  let rec aux tr accum =
    match tr with
    | Leaf (char, _) -> 
        if char = elem then accum
        else ""
    | Node (prio, left, right) ->
        aux left (accum ^ "0") ^ aux right (accum ^ "1")
  in
  aux tr ""

let huffman lst =
  let tr = Huffman.huffman_make lst in
  let chars = List.map fst lst in
  let traverse_inner a b = binary_traverse b a in
  let bin = List.map (traverse_inner tr) @@ chars in
  List.combine chars bin
