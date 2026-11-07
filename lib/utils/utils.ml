module type Addr = sig
  type t

  val zero : t
  val next : t -> t
  val compare : t -> t -> int
  val pp : t Fmt.t
end

module Mem = struct
  module Make (Addr : Addr) = struct
    module M = Map.Make (Addr)

    type 'a t = { map : 'a M.t; next_addr : Addr.t }

    let empty = { map = M.empty; next_addr = Addr.zero }

    let alloc v m =
      ( { map = M.add m.next_addr v m.map; next_addr = Addr.next m.next_addr },
        m.next_addr )

    let find_opt addr m = M.find_opt addr m.map

    let add addr v m =
      { map = M.add addr v m.map; next_addr = max m.next_addr (Addr.next addr) }
  end
end

module Env = Map.Make (String)

exception Unreachable of string

let id x = x
let ( >> ) f g x = g (f x)
let unreachable fmt = Fmt.kstr (fun msg -> raise (Unreachable msg)) fmt

let result_foldM f acc lst =
  let rec aux acc lst =
    match lst with
    | [] -> Result.ok acc
    | x :: xs -> (
        match f acc x with Error e -> Result.error e | Ok acc' -> aux acc' xs)
  in
  aux acc lst

let shortest_float_string f =
  let rec go p =
    let s = Printf.sprintf "%.*g" p f in
    if p >= 17 || float_of_string s = f then s else go (p + 1)
  in
  let s = go 1 in
  if String.exists (fun c -> c = '.' || c = 'e' || c = 'n') s then s
  else s ^ ".0"
