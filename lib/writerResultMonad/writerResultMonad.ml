type ('a, 'e, 'b) t = 'b list * int * int -> ('a, 'e) result * ('b list * int * int)

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

let tell x (log, log_id, call_id) = (Ok (), (x :: log, log_id + 1, call_id))
let get_log_id () (log, log_id, call_id) = (Ok log_id, (log, log_id, call_id))
let incr_call_id () (log, log_id, call_id) = (Ok call_id, (log, log_id, call_id + 1))

let run m =
  let r, (log, _, _) = m ([], 0, 0) in
  (r, List.rev log)

module Syntax = struct
  let ( let* ) m f = bind m f
  let ( >>= ) m f = bind m f
  let ( =>> ) m f = map m f
  let ( =&> ) m f = map_error m f
end
