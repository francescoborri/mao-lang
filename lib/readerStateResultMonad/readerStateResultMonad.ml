type 'b state = { logs : 'b list; log_id : int; call_id : int; depth : int }

(* ['r] is a read-only environment, ['b] the type of the logs *)
type ('a, 'e, 'r, 'b) t = 'r -> 'b state -> ('a, 'e) result * 'b state

let ok x _ st = (Ok x, st)
let error e _ st = (Error e, st)
let return e _ st = (e, st)
let ask () env st = (Ok env, st)

let bind m f env st =
  match m env st with Ok x, st -> f x env st | Error e, st -> (Error e, st)

let map m f env st =
  match m env st with Ok x, st -> (Ok (f x), st) | Error e, st -> (Error e, st)

let map_error m f env st =
  match m env st with Ok x, st -> (Ok x, st) | Error e, st -> (Error (f e), st)

let tell x _ st =
  (Ok (), { st with logs = x :: st.logs; log_id = st.log_id + 1 })

let get_log_id () _ st = (Ok st.log_id, st)
let incr_call_id () _ st = (Ok st.call_id, { st with call_id = st.call_id + 1 })

let enter_call () _ st =
  let depth = st.depth + 1 in
  (Ok depth, { st with depth })

let exit_call () _ st = (Ok (), { st with depth = st.depth - 1 })

let run ~env m =
  let r, { logs } = m env { logs = []; log_id = 0; call_id = 0; depth = 0 } in
  (r, List.rev logs)

module Syntax = struct
  let ( let* ) m f = bind m f
  let ( >>= ) m f = bind m f
  let ( =>> ) m f = map m f
  let ( =&> ) m f = map_error m f
end
