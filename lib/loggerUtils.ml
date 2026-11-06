type ('a, 'e, 'l) t = ('a, 'e) result * 'l

let return x = (Ok x, [])
let fail e = (Error e, [])
let tell msg = (Ok (), [ msg ])

let bind m f =
  match m with
  | Error e, log -> (Error e, log)
  | Ok x, log ->
      let r, log' = f x in
      (r, log @ log')

module Syntax = struct
  let ( let* ) m f = bind m f
end
