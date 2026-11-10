type ('a, 'e, 'b) t = 'b list * int -> ('a, 'e) result * ('b list * int)

let ok x log = (Ok x, log)
let error e log = (Error e, log)

let bind m f log =
  match m log with Ok x, log -> f x log | Error e, log -> (Error e, log)

let map m f log =
  match m log with
  | Ok x, log -> (Ok (f x), log)
  | Error e, log -> (Error e, log)

let map_error m f log =
  match m log with
  | Ok x, log -> (Ok x, log)
  | Error e, log -> (Error (f e), log)

let tell x (log, cnt) = (Ok (), (x :: log, cnt + 1))
let get_log_id () (log, cnt) = (Ok cnt, (log, cnt))

let run m =
  let r, (log, _) = m ([], 0) in
  (r, List.rev log)

module Syntax = struct
  let ( let* ) m f = bind m f
  let ( >>= ) m f = bind m f
  let ( =>> ) m f = map m f
  let ( =&> ) m f = map_error m f
end
