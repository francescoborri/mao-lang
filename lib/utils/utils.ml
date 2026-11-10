exception Unreachable of string

let ( >> ) f g x = g (f x)
let ( *** ) f g (x, y) = (f x, g y)
let unreachable fmt = Fmt.kstr (fun msg -> raise (Unreachable msg)) fmt

let result_fold_leftM f acc lst =
  let rec aux acc l =
    match l with
    | [] -> Result.ok acc
    | x :: xs -> (
        match f acc x with Error e -> Result.error e | Ok acc -> aux acc xs)
  in
  aux acc lst

let result_fold_left2M f acc lst1 lst2 =
  let rec aux acc lst1 lst2 =
    match (lst1, lst2) with
    | [], [] -> Result.ok acc
    | x :: xs, y :: ys -> (
        match f acc x y with
        | Error e -> Result.error e
        | Ok acc' -> aux acc' xs ys)
    | _ -> unreachable "result_fold_left2M: lists have different lengths"
  in
  aux acc lst1 lst2

let shortest_float_string f =
  let rec go p =
    let s = Printf.sprintf "%.*g" p f in
    if p >= 17 || float_of_string s = f then s else go (p + 1)
  in
  let s = go 1 in
  if String.exists (fun c -> c = '.' || c = 'e' || c = 'n') s then s
  else s ^ ".0"
