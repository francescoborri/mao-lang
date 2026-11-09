type 'l dlist = 'l list -> 'l list
type ('a, 'e, 'l) t = ('a, 'e) result * 'l dlist

let empty = Fun.id
let append left right tail = left (right tail)
let ok x = (Ok x, empty)
let error e = (Error e, empty)
let tell msg = (Ok (), fun tail -> msg :: tail)

let bind (result, log) f =
  match result with
  | Error e -> (Error e, log)
  | Ok x ->
      let result', log' = f x in
      (result', append log log')

let run (result, log) = (result, log [])

module Syntax = struct
  let ( let* ) m f = bind m f
end
