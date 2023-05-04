(*99 OCaml Problems*)

(*Problem 1*)
(*Tail of a List*)
let rec tail lst =
  match lst with
  | [] -> None
  | [x] -> Some x
  | _::t -> tail t

(*Problem 2*)
(*Last two elements of a list*)
let rec last_two lst =
  match lst with
  | [] -> None | [_] -> None
  | [x;y] -> Some (x,y)
  | _::t -> last_two t

(*Problem 3*)
(*Nth element of a list*)
let rec nth lst n =
  match lst with
  | h::_ when n = 1 -> Some h
  | _ ::t when n > 1 -> nth t (n-1)
  | _ -> None

(*Problem 4*)
(*Length of a list*)
let length lst =
  let rec length_inner lst accum =
    match lst with
    | [] -> accum
    | h::t -> length_inner t (accum+1)
  in
  length_inner lst 0

(*Problem 5*)
(*Reverse a list*)
let reverse lst =
  let rec reverse_inner lst accum =
    match lst with
    | [] -> accum
    | h::t -> reverse_inner t (h::accum)
  in
  reverse_inner lst []

(*Problem 6*)
(*Palindrome*)
let is_palindrome lst =
  if lst = (reverse lst) then true
  else false

(*Problem 7*)
(*Flatten a list*)
type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten lst =
  let rec flatten_inner lst accum =
    match lst with
    | [] -> accum
    | One h:: t -> flatten_inner t (h::accum)
    | Many h::t -> flatten_inner t (flatten_inner h accum)
  in
  reverse @@ flatten_inner lst []

(*Problem 8*)
(*Eliminate duplicates*)
let compress lst =
  let rec compress_inner lst accum =
    match (lst, accum) with
    | ([], _) -> accum
    | (h::t, []) -> compress_inner t (h::accum)
    | (h::t, x::xs) ->
        if h = x then compress_inner t (accum)
        else compress_inner t (h::accum)
  in
  reverse @@ compress_inner lst []


