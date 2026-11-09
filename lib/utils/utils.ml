module type Addr = sig
  type t

  val zero : t
  val next : t -> t
  val compare : t -> t -> int
  val pp : t Fmt.t
end

module Mem = struct
  module type S = sig
    type addr
    type 'a t

    val empty : 'a t
    val alloc : 'a -> 'a t -> 'a t * addr
    val find_opt : addr -> 'a t -> 'a option
    val add : addr -> 'a -> 'a t -> 'a t
    val pp : 'a Fmt.t -> 'a t Fmt.t
  end

  module Make (Addr : Addr) : S with type addr = Addr.t = struct
    module M = Map.Make (Addr)

    type addr = Addr.t
    type 'a t = { map : 'a M.t; next_addr : Addr.t }

    let empty = { map = M.empty; next_addr = Addr.zero }

    let alloc v m =
      ( { map = M.add m.next_addr v m.map; next_addr = Addr.next m.next_addr },
        m.next_addr )

    let find_opt addr m = M.find_opt addr m.map

    let add addr v m =
      { map = M.add addr v m.map; next_addr = max m.next_addr (Addr.next addr) }

    let pp pp_v fmt m =
      let pp_binding fmt (addr, v) =
        Fmt.pf fmt "%a -> %a" Addr.pp addr pp_v v
      in
      Fmt.pf fmt "{@[<v>%a@]}"
        Fmt.(list ~sep:(any ";@ ") pp_binding)
        (M.bindings m.map)
  end
end

module Env = Map.Make (String)

exception Unreachable of string

let id x = x
let ( >> ) f g x = g (f x)
let unreachable fmt = Fmt.kstr (fun msg -> raise (Unreachable msg)) fmt

let result_fold_leftM f acc lst =
  let rec aux acc lst =
    match lst with
    | [] -> Result.ok acc
    | x :: xs -> (
        match f acc x with Error e -> Result.error e | Ok acc' -> aux acc' xs)
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
    | _ -> failwith "result_fold_left2M: lists have different lengths"
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
