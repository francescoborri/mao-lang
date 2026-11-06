open MaoAst_pp
open Utils

let unreachable fmt =
  Fmt.kstr ((fun msg -> Fmt.str "[Unreachable] %s" msg) >> failwith) fmt

let located_msg ~loc fmt =
  Fmt.kstr (fun msg -> Fmt.str "%s at %a" msg Loc.pp_start_pos loc) fmt
