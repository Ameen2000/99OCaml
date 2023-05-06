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

(*Problem 9*)
(*Pack Consecutive Duplicates*)
let pack lst =
  let rec packer lst acc1 acc2 =
    match lst with
    | [] -> []
    | [x] -> (x::acc1)::acc2
    | h::(m::_ as t) ->
        if h = m then packer t (h::acc1) acc2
        else packer t [] ((h::acc1)::acc2)
  in
  reverse @@ packer lst [] []

(*Problem 10*)
(*Run-Length Encoding*)
let encode lst =
  let rec encode_inner lst acc count =
    match lst with
    | [] -> []
    | [x] -> (count+1, x) :: acc
    | h::(m::_ as t) ->
        if h = m then encode_inner t acc (count+1)
        else encode_inner t ((count+1, h)::acc) 0
  in
  reverse @@ encode_inner lst [] 0

(*Problem 11*)
(*Modified Run-Length Encoding*)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let mencode (lst: 'a list) =
  let record count elem =
    if count = 1 then One elem
    else Many (count, elem) in
  let rec mencode_inner lst acc count =
    match lst with
    | [] -> []
    | [x] -> (record (count+1) x) :: acc
    | h::(m::_ as t) ->
        if h = m then mencode_inner t acc (count+1)
        else mencode_inner t ((record (count+1) h) :: acc) 0
  in
  reverse @@ mencode_inner lst [] 0

(*Problem 12*)
(*Decode a Run-Length Encoding*)
let decode lst =
  let rec many acc count elem =
    if count = 0 then acc
    else  many (elem :: acc) (count-1) elem in
  let rec decode_inner lst acc =
    match lst with
    | [] -> acc
    | (One h)::t -> decode_inner t (h::acc)
    | (Many (count, h))::t -> decode_inner t (many acc count h)
  in
  decode_inner (reverse lst) []

(*Problem 13*)
(*Duplicate elements of a list*)
let duplicate lst =
  let rec duplicate_inner lst accum =
    match lst with
    | [] -> accum
    | h::t -> duplicate_inner t (h::(h::accum))
  in
  reverse @@ duplicate_inner lst []

(*Problem 14*)
(*Replicate the Elements of a List a 
 Given Number of times*)
let replicate lst n =
  let rec copier n accum elem =
    if n < 1 then accum
    else copier (n-1) (elem::accum) elem in
  let rec replicate_inner lst accum =
    match lst with
    | [] -> accum
    | h::t -> replicate_inner t (copier n accum h)
  in
  reverse @@ replicate_inner lst []
    
(*Problem 15*)
(*Drop every N'th element of a list*)
let drop lst n =
  let rec dropper i lst =
    match lst with
    | [] -> []
    | h::t ->
        if n = i then dropper 1 t
        else h :: dropper (i+1) t
  in
  dropper 1 lst

(*Problem 16*)
(*Split a list into two parts; the length of the first
 part is given*)
let split lst n =
  let rec split_inner lst n accum =
    match lst with
    | [] -> (reverse accum, lst)
    | h::t ->
        if n = 0 then (reverse accum, lst)
        else split_inner t (n-1) (h::accum)
  in
  split_inner lst n []

(*Problem 17*)
(*Extract a slice from a list*)
let slice lst i k =
  let rec slicer lst j accum =
    match lst with
    | [] -> accum
    | _ when j > k -> accum
    | h::t ->
        if (j >= i && j <= k) then slicer t (j+1) (h::accum)
        else slicer t (j+1) (accum)
  in
  reverse @@ slicer lst 0 []

(*Problem 18*)
(*Rotate a list N places to the left*)
let rotate lst n =
  let nlst = (length lst) in
  let rec rotate_inner lst n accum =
    match lst with
    | [] -> accum
    | h::t ->
        if n = 0 then lst @ (reverse accum)
        else rotate_inner t (n-1) (h::accum)
  in
  rotate_inner lst (n mod nlst) []

(*Problem 19*)
(*Remove the Kth Element from a list*)
let remove_at k lst =
  let rec remove_at_inner i lst accum =
    match lst with
    | [] -> reverse accum
    | h::t ->
        if i = k then (reverse accum) @ t
        else remove_at_inner (i+1) t (h::accum)
  in
  remove_at_inner 0 lst []

(*Problem 20*)
(*Insert an Element at a Given Position Into a List*)
let insert_at elem k lst =
  let rec insert_at_inner elem i lst accum =
    match lst with
    | [] -> reverse (elem::accum)
    | h::t ->
        if i = k then (reverse accum) @ (elem::h::t)
        else insert_at_inner elem (i+1) t (h::accum)
  in
  insert_at_inner elem 0 lst []
