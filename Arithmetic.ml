let range a b =
  let rec range_inner a b accum =
    match (a < b) with
    | false ->
        if a = b then b::accum
        else range_inner (a-1) b (a::accum)
    | true -> range_inner (a+1) b (a::accum)
  in
  List.rev @@ range_inner a b []

(*Problem 29*)
(*Determine whether a given integer is prime*)
let is_prime n =
  let rec fPrime_inner n start =
    match (n > start*start, n mod start) with
      | (false, 0) -> false
      | (false, _) -> true
      | (_, 0) -> false
      | (_, _) -> fPrime_inner n (start+1)
  in
  let fPrime n = fPrime_inner n 2
  in
  n = 2 || (n > 2) && (n mod 2 = 1) && fPrime n

(*Problem 30*)
(*Determine the greatest common divisor of two positive integer numbers*)
let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)

(*Problem 31*)
(*Determine whether two positive Integer number are coprime*)
let coprime a b =
  if (gcd a b) = 1 then true else false

(*Problem 32*)
(*Calculate Euler's Totient function phi(m)*)
let phi m =
  let lst = range 1 (m-1) in
  List.length @@ List.filter (coprime m) lst

(*Problem 33*)
(*Determine the Prime Factors of a Given Positive Integer*)
let pfactors n =
  let rec pfactors_inner divisor accum n =
    if n = 1 then accum
    else 
      if n mod divisor = 0
      then pfactors_inner divisor (divisor::accum) (n / divisor)
      else pfactors_inner (divisor+1) accum n
  in
  pfactors_inner 2 [] n

(*Problem 34*)
let pmfactors n =
  let rec pmfactors_inner divisor accum n =
    if n = 1 then accum
    else
      if n mod divisor = 0
      then 
        match pmfactors_inner divisor accum (n / divisor) with
        | (p, m)::t when p = divisor -> (p, m+1)::t
        | lst -> (divisor, 1)::lst
      else pmfactors_inner (divisor+1) accum n
  in
  pmfactors_inner 2 [] n

(*Problem 35*)
(*Improve Euler's Totient function*)
let pow n e =
  let rec pow_inner n e accum =
    if e < 1 then accum
    else pow_inner n (e-1) (n*accum)
  in
  pow_inner n e 1

let phi_improved n =
  let terms init (p, m) =
    init*(p-1)*(pow p (m-1))
  in
  List.fold_left terms 1 (pmfactors n)

(*Problem 36*)
(*Time both versions of the Totient function*)
let timeit f x =
  let start = Unix.gettimeofday () in
  let result = f x in
  let stop = Unix.gettimeofday () in
  Printf.printf "Time: %fs\n" (stop -. start);
  Printf.printf "Result: %d\n" result

(*Problem 37*)
(*A List of Prime Numbers*)
let all_primes a b =
  List.filter is_prime (range a b)

(*Problem 38*)
(*Goldbach's Conjecture*)
let goldbach n =
  if n = 2 || (n mod 2 = 1)
  then (1, n-1)
  else
  let rec goldbach_inner a b =
  match (a < b) with
  | false -> (a,b)
  | true ->
          if is_prime a && is_prime b
          then (a, b)
          else goldbach_inner (a+1) (b-1)
  in
  goldbach_inner 3 (n-3)

(*Problem 39*)
(*A List of Goldbach Compositions*)
let goldbach_lst a b =
  let evens = List.filter (fun x -> x mod 2 = 0 && x > 2) (range a b) in
  let goldbach_compositions = List.map goldbach evens in
  List.combine evens goldbach_compositions

(*Very rarely, the primes are both bigger than say 50. 
Try to find out how many such cases there are in the range 2..3000.*)
let goldbach_bound a b lim =
  let checker (x, (y,z)) = y > lim && z > lim in
  let goldbach_compositions = goldbach_lst (2*lim+2) b in
  List.filter checker goldbach_compositions

