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
  | [] -> None
  | h::t -> 
      if n = 0 then Some h
      else nth t (n-1)

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
(*Run-Lenght Encoding Direct Solution*)
(*Already solved directly in problem 11*)

(*Problem 13*)
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

(*Problem 14*)
(*Duplicate elements of a list*)
let duplicate lst =
  let rec duplicate_inner lst accum =
    match lst with
    | [] -> accum
    | h::t -> duplicate_inner t (h::(h::accum))
  in
  reverse @@ duplicate_inner lst []

(*Problem 15*)
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
    
(*Problem 16*)
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

(*Problem 17*)
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

(*Problem 18*)
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

(*Problem 19*)
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

(*Problem 20*)
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

(*Problem 21*)
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

(*Problem 22*)
(*Create a list Containing All Integers Within a Given Range*)
let range a b =
  let rec range_inner a b accum =
    match (a < b) with
    | false ->
        if a = b then b::accum
        else range_inner (a-1) b (a::accum)
    | true -> range_inner (a+1) b (a::accum)
  in
  reverse @@ range_inner a b []

(*Problem 23*)
(*Extract a Given Number of Randomly Selected Elements
 from a List*)
exception Nonsense of string

let rand_select lst n =
  let rec rand_select_inner lst n accum =
    let nlst = length lst in
    let selection =
      if nlst > 0 then Random.int nlst
      else 0 in
    match n, lst with
    | _, [] -> accum
    | 0, _ -> accum
    | _ -> rand_select_inner (remove_at selection lst) (n-1) ((nth lst @@ selection) :: accum)
  in 
  if n < 0 then raise (Nonsense "Impossible try again")
  else List.filter_map (fun x -> x) @@ rand_select_inner lst n []

(*Problem 24*)
(*Lotto: Draw N Different Random Numbers From the
 Set 1..M*)
let lotto_select n bound =
  rand_select (range 1 bound) n

(*Problem 25*)
(*Generate a random permutation of the elements of a list*)
let permutation lst =
  rand_select lst (length lst)

(*Problem 26*)
(*Generate the Combinations of K Distinct Objects Chosen
 From the N Elements of a List*)
let rec combos k lst =
  if k <= 0 then [[]]
  else match lst with
  | [] -> []
  | h::t ->
      let fix_h = List.map (fun x -> h::x) (combos (k-1) t) in
      let pivot = combos k t in
      fix_h @ pivot

(*Problem 27*)
(*Group the Elements of a Set into Disjoint Subsets*)
(*Not answered yet*)

(*Problem 28*)
(*Sorting a List of Lists According to Length of Sublists*)
(*Part 1*)
let length_sort lst =
  let lengths = List.map length lst in
  let pairing = List.combine lst lengths in
  let comparer (x1, y1) (x2, y2) = compare y1 y2 in
  let sorted = List.sort comparer pairing in
  List.map fst sorted

(*Part 2*)
let frequency_sort lst =
  let lengths = List.map length lst in
  let rec frequencies n lst elem =
    match lst with
    | [] -> n
    | h::t ->
        if elem = h then frequencies (n+1) t elem
        else frequencies n t elem
  in
  let frequencies_lst = List.map (frequencies 0 lengths) lengths in
  let pairing = List.combine lst frequencies_lst in
  let comparer (x1, y1) (x2, y2) = compare y1 y2 in
  let sorted = List.sort comparer pairing in
  List.map fst sorted
