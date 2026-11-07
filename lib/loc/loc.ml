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

let pp pp fmt { it; at } =
  Fmt.pf fmt "%a at line %d, column %d" pp it at.start_pos.line at.start_pos.col
