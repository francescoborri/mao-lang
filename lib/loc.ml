open Fmt

type pos = { line : int; col : int; offset : int }
type loc = { start_pos : pos; end_pos : pos }
type 'a t = { it : 'a; at : loc }

let located ~loc item = { it = item; at = loc }
let item { it; _ } = it
let loc { at; _ } = at
let unpack { it; at } = (it, at)

let located_with_positions ~start_pos ~end_pos item =
  let open Lexing in
  located item
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

let pp_pos fmt pos = Fmt.pf fmt "line %d, column %d" pos.line pos.col
let pp_start_pos fmt loc = pp_pos fmt loc.start_pos
