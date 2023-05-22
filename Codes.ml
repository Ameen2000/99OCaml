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