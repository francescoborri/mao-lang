let ( >> ) f g x = g (f x)

module Env = Map.Make (String)

module Addr : sig
  type t

  val fresh : unit -> t
  val of_int : int -> t
  val add : t -> int -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : t Fmt.t
end = struct
  type t = int

  let counter = Atomic.make 0
  let fresh () = Atomic.fetch_and_add counter 1
  let of_int i = i
  let add a b = a + b
  let equal = Int.equal
  let compare = Int.compare
  let pp fmt a = Fmt.pf fmt "@%d" a
end

module AddrMap = Map.Make (Addr)

let shortest_float_string f =
  let rec go p =
    let s = Printf.sprintf "%.*g" p f in
    if p >= 17 || float_of_string s = f then s else go (p + 1)
  in
  let s = go 1 in
  if String.exists (fun c -> c = '.' || c = 'e' || c = 'n') s then s
  else s ^ ".0"
