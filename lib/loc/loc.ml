open Fmt

type pos = { line : int; col : int; offset : int }
type loc = { start_pos : pos; end_pos : pos }
type 'a t = { it : 'a; at : loc }

let get_item { it; _ } = it
let get_loc { at; _ } = at
let map_item f { it; at } = { it = f it; at }
let with_loc ~loc item = { it = item; at = loc }

let with_positions ~start_pos ~end_pos item =
  let open Lexing in
  with_loc item
    ~loc:
      {
        start_pos =
          {
            line = start_pos.pos_lnum;
            col = start_pos.pos_cnum - start_pos.pos_bol + 1;
            offset = start_pos.pos_cnum;
          };
        end_pos =
          {
            line = end_pos.pos_lnum;
            col = end_pos.pos_cnum - end_pos.pos_bol + 1;
            offset = end_pos.pos_cnum;
          };
      }

let pp_loc fmt { start_pos } =
  Fmt.pf fmt "line %d, column %d" start_pos.line start_pos.col

let pp_end_loc fmt { end_pos } =
  Fmt.pf fmt "line %d, column %d" end_pos.line end_pos.col

let pp_loc_full fmt loc = Fmt.pf fmt "%a - %a" pp_loc loc pp_end_loc loc
let pp pp fmt { it; at } = Fmt.pf fmt "%a at %a" pp it pp_loc at
let pp_full pp fmt { it; at } = Fmt.pf fmt "%a at %a" pp it pp_loc_full at
