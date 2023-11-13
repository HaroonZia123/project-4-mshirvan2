open Funs

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)
let rev_tup (a, b, c) = (c, b, a)
let is_even x = x mod 2 = 0
let volume (x1, y1, z1) (x2, y2, z2) =
  let length = abs (x2 - x1) in
  let width = abs (y2 - y1) in
  let height = abs (z2 - z1) in
  length * width * height


(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)
let rec fibonacci n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fibonacci (n - 1) + fibonacci (n - 2)

let rec log x y =
  if y < x then 0
  else 1 + log x (y / x)

let rec gcf x y =
  if y = 0 then x
  else gcf y (x mod y)

let rec maxFuncChain init funcs =
  match funcs with
  | [] -> init
  | f :: rest ->
    let result = f init in
    max (maxFuncChain init rest) (maxFuncChain result rest)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec reverse lst =
  match lst with
  | [] -> []
  | x :: xs -> reverse xs @ [x]

(*
let rec zip lst1 lst2 =
  match (lst1, lst2) with
  | ([], _) | (_, []) -> []
  | (x1 :: xs1, x2 :: xs2) -> (x1, x2) :: zip xs1 xs2
*)
let rec is_palindrome lst =
  let rec reverse_and_check original reversed =
    match (original, reversed) with
    | ([], []) -> true
    | (x1 :: xs1, x2 :: xs2) when x1 = x2 -> reverse_and_check xs1 xs2
    | _ -> false
  in
  reverse_and_check lst (reverse lst)

let is_prime n =
  if n <= 1 then false
  else
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
    in
    is_not_divisor 2

let rec square_primes lst =
  match lst with
  | [] -> []
  | x :: xs when is_prime x -> (x, x * x) :: square_primes xs
  | _ :: xs -> square_primes xs

let rec partition p lst =
  match lst with
  | [] -> ([], [])
  | x :: xs ->
    let (l1, l2) = partition p xs in
    if p x then (x :: l1, l2)
    else (l1, x :: l2)


    let rec zip lst1 lst2 =
      match (lst1, lst2) with
      | ([], _) | (_, []) -> []
      | ((a, b) :: rest1, (c, d) :: rest2) ->
        (a, b, c, d) :: zip rest1 rest2
    

(*****************)
(* Part 4: HOF *)
(*****************)
let is_present lst x =
  let is_equal_to_x element = if element = x then 1 else 0 in
  map is_equal_to_x lst

let count_occ lst target =
  let count_if_equal count element = if element = target then count + 1 else count in
  fold count_if_equal 0 lst


  



let ap fns args =
  List.flatten (List.map (fun f -> List.map f args) fns)



let addgenerator x = fun y -> x + y

let rec remove_duplicates lst seen =
  match lst with
  | [] -> []
  | x :: xs ->
    if List.mem x seen then remove_duplicates xs seen
    else x :: remove_duplicates xs (x :: seen)

let uniq lst = remove_duplicates lst []

let temp lst1 lst2 =
  let rec aux lst1 lst2 xs ys =
    match lst1, lst2 with
    | [], [] -> (List.rev xs, List.rev ys)
    | (x1, _) :: tl1, (_, y2) :: tl2 ->
      aux tl1 tl2 (x1 :: xs) (y2 :: ys)
    | _ -> failwith "Input lists must have the same length"
  in
  aux lst1 lst2 [] []


let alternating_elements1 lst1 lst2 =
  let rec aux index acc lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> List.rev acc
    | x1 :: tl1, x2 :: tl2 ->
      if index mod 2 = 0 then
        aux (index + 1) (x2 :: acc) tl1 tl2
      else
        aux (index + 1) (x1 :: acc) tl1 tl2
    | _ -> failwith "Input lists must have the same length"
  in
  aux 0 [] lst1 lst2

let alternating_elements2 lst1 lst2 =
  let rec aux index acc lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> List.rev acc
    | x1 :: tl1, x2 :: tl2 ->
      if index mod 2 = 0 then
        aux (index + 1) (x1 :: acc) tl1 tl2
      else
        aux (index + 1) (x2 :: acc) tl1 tl2
    | _ -> failwith "Input lists must have the same length"
  in
  aux 0 [] lst1 lst2

let jumping_tuples lst1 lst2 =
  let xs, ys = temp lst1 lst2 in
  let result1 = alternating_elements1 xs ys in
  let result2 = alternating_elements2 xs ys in
  result1 @ result2