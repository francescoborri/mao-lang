exception Unreachable of string

let ( >> ) f g x = g (f x)
let ( *** ) f g (x, y) = (f x, g y)
let unreachable fmt = Fmt.kstr (fun msg -> raise (Unreachable msg)) fmt

let result_fold_rightM f lst acc =
  let rec aux acc l =
    match l with
    | [] -> Result.ok acc
    | x :: xs -> (
        match aux acc xs with Error e -> Result.error e | Ok acc -> f x acc)
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
    | _ -> unreachable "result_fold_left2M: liste di lunghezza diversa"
  in
  aux acc lst1 lst2

let result_fold_right2M f lst1 lst2 acc =
  let rec aux lst1 lst2 acc =
    match (lst1, lst2) with
    | [], [] -> Result.ok acc
    | x :: xs, y :: ys -> (
        match aux xs ys acc with
        | Error e -> Result.error e
        | Ok acc' -> f x y acc')
    | _ -> unreachable "result_fold_right2M: liste di lunghezza diversa"
  in
  aux lst1 lst2 acc

(* number of code points, skipping UTF-8 continuation bytes *)
let utf8_length =
  String.fold_left
    (fun n c -> if Char.code c land 0xC0 = 0x80 then n else n + 1)
    0

let shortest_float_string f =
  let rec shortest_exp p =
    let s = Fmt.str "%.*e" p f in
    if p >= 16 || float_of_string s = f then s else shortest_exp (p + 1)
  in
  if Float.is_nan f then "NaN"
  else if not (Float.is_finite f) then
    if f > 0.0 then "+\u{221E}" else "-\u{221E}"
  else
    let s = shortest_exp 0 in
    let e_idx = String.index s 'e' in
    let exp =
      int_of_string (String.sub s (e_idx + 1) (String.length s - e_idx - 1))
    in
    let digits =
      String.sub s 0 e_idx |> String.to_seq
      |> Seq.filter (fun c -> c >= '0' && c <= '9')
      |> String.of_seq
    in
    let sign = if s.[0] = '-' then "-" else "" in
    let point = exp + 1 and n = String.length digits in
    let plain =
      if point <= 0 then "0." ^ String.make (-point) '0' ^ digits
      else if point >= n then digits ^ String.make (point - n) '0' ^ ".0"
      else String.sub digits 0 point ^ "." ^ String.sub digits point (n - point)
    in
    sign ^ plain
