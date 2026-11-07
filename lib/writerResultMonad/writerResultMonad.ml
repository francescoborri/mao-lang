type ('a, 'e, 'l) t = ('a, 'e) result * 'l list

let ok x = (Ok x, [])
let error e = (Error e, [])
let log msg = (Ok (), [ msg ])

let bind m f =
  match m with
  | Error e, log -> (Error e, log)
  | Ok x, log ->
      let r, log' = f x in
      (r, log @ log')

let foldM f acc ms =
  let rec aux acc ms log =
    match ms with
    | [] -> (Ok acc, log)
    | m :: ms -> (
        match f acc m with
        | Error e, log' -> (Error e, log @ log')
        | Ok acc', log' -> aux acc' ms (log @ log'))
  in
  aux acc ms []

module Syntax = struct
  let ( let* ) m f = bind m f
end
