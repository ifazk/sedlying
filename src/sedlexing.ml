type lybuf = {
  gen: [`LastUchar of Uchar.t | `Uchar of Uchar.t] Gen.t;

  mutable buf: Uchar.t array; (* codepoints buffer *)
  mutable bounds: Bitv.t; (* bitvector denoting boundaries
                           * 1 means that codepoint ends a grapheme cluster
                           * must be same size as buf *)

  mutable len: int;    (* Number of meaningful codepoints entered into buf *)
  (* contents of the buffer are from start_pos to len *)
  mutable pos: int;    (* pos is the current_position in the buffer *)

  (* lexing positions calculations use this offset *)
  mutable offset: int; (* Position of the first codepoint in buffer in the input stream *)
  (* lexeme curr_pos = pos + offset *)
  (* lexeme start_pos = start_pos + offset *)

  (* lexing bol and line *)
  mutable curr_bol: int; (* bol is the index in the input stream but not buffer *)
  mutable curr_line: int; (* start from 1, if it is 0, we would not track postion info for you *)

  (* state at the start of lexing, used for full rollback *)
  mutable start_pos: int; (* First char we need to keep visible *)
  mutable start_bol: int;
  mutable start_line: int;

  (* state at the last useful final state, used for backtracking *)
  mutable marked_pos: int;
  mutable marked_bol: int;
  mutable marked_line: int;
  mutable marked_val: int; (* the final state, -1 means no final states were stored *)

  mutable filename: string;

  mutable finished: bool;
}

let empty_lexbuf = {
  gen = Gen.empty;
  buf = [| Uchar.of_int 0 |];
  bounds = Bitv.create 1 false;
  len = 0;
  offset = 0;
  pos = 0;
  curr_bol = 0;
  curr_line = 0;
  start_pos = 0;
  start_bol = 0;
  start_line = 0;
  marked_pos = 0;
  marked_bol = 0;
  marked_line = 0;
  marked_val = -1;
  filename = "";
  finished = false;
}

let chunk_size = 512

let create ?(filename="") gen = {
  empty_lexbuf with
    gen;
    buf = Array.make chunk_size (Uchar.of_int 0);
    bounds = Bitv.create chunk_size false;
    curr_line = 1;
    filename;
}

let shift_left lexbuf =
  let start_pos = lexbuf.start_pos in
  let size = lexbuf.len - start_pos in
  (* shiftl moves bits towards more significant digits
   * with the 0-th bit being least significant
   * so we must use shilfr *)
  ( lexbuf.bounds <- Bitv.shiftr lexbuf.bounds start_pos
  ; Array.blit lexbuf.buf start_pos lexbuf.buf 0 size
  ; lexbuf.len <- size
  ; lexbuf.start_pos <- 0
  ; lexbuf.offset <- lexbuf.offset + start_pos
  ; lexbuf.pos <- lexbuf.pos - start_pos
  ; lexbuf.marked_pos <- lexbuf.marked_pos - start_pos
  )

let expand_right lexbuf =
  let start_pos = lexbuf.start_pos in
  let old_arr = lexbuf.buf in
  let old_len = Array.length old_arr in
  let size = old_len - start_pos in
  let new_len = old_len * 2 in
  let new_arr = Array.make new_len (Uchar.of_int 0) in
  let new_bounds = Bitv.create new_len false in
  ( Array.blit old_arr start_pos new_arr start_pos size
  ; lexbuf.buf <- new_arr
  ; Bitv.blit lexbuf.bounds start_pos new_bounds start_pos size
  ; lexbuf.bounds <- new_bounds
  )

let shift_left_and_expand_right lexbuf =
  let start_pos = lexbuf.start_pos in
  let size = lexbuf.len - start_pos in
  let old_arr = lexbuf.buf in
  let old_arr_len = Array.length old_arr in
  let new_arr_len = old_arr_len * 2 in
  let new_arr = Array.make new_arr_len (Uchar.of_int 0) in
  let new_bounds = Bitv.create new_arr_len false in
  ( Array.blit old_arr start_pos new_arr 0 size
  ; Bitv.blit lexbuf.bounds start_pos new_bounds 0 size
  ; lexbuf.buf <- new_arr
  ; lexbuf.bounds <- new_bounds
  ; lexbuf.len <- size
  ; lexbuf.start_pos <- 0
  ; lexbuf.offset <- lexbuf.offset + start_pos
  ; lexbuf.pos <- lexbuf.pos - start_pos
  ; lexbuf.marked_pos <- lexbuf.marked_pos - start_pos
  )

let new_line_chars =
  [ Uchar.of_int 0x000A (* line feed *)
  ; Uchar.of_int 0x000B (* vertical tab *)
  ; Uchar.of_int 0x000C (* form feed *)
  ; Uchar.of_int 0x000D (* carriage return *)
  ; Uchar.of_int 0x0085 (* next line *)
  ; Uchar.of_int 0x2028 (* line separator *)
  ; Uchar.of_int 0x2029 (* paragraph separator *)
  ]

let uchar_of_char = function
  | `LastUchar c -> c
  | `Uchar c -> c
let char_at_boundary = function
  | `LastUchar _ -> true
  | `Uchar _ -> false

let rec add_char ~char lexbuf : unit =
  let buf = lexbuf.buf in
  let len = lexbuf.len in
  if len < Array.length buf then
    ( Array.set buf len (uchar_of_char char)
    ; Bitv.set lexbuf.bounds len (char_at_boundary char)
    ; lexbuf.len <- len + 1
    )
  else
    let start_pos = lexbuf.start_pos in
    let () =
      if start_pos = 0 then
        expand_right lexbuf
      else if chunk_size < start_pos then
        shift_left lexbuf
      else
        shift_left_and_expand_right lexbuf
    in
      add_char ~char lexbuf

let rec refill lexbuf n =
  if n <= 0 then
    ()
  else
    match Gen.next lexbuf.gen with
    | Some char -> (add_char ~char lexbuf; refill lexbuf (n - 1))
    | None -> lexbuf.finished <- true

let new_line lexbuf =
  if lexbuf.curr_line != 0 then
  lexbuf.curr_line <- lexbuf.curr_line + 1;
  lexbuf.curr_bol <- lexbuf.pos + lexbuf.offset

let next lexbuf =
  if (not lexbuf.finished) && (lexbuf.pos = lexbuf.len) then refill lexbuf chunk_size;
  if lexbuf.finished && (lexbuf.pos = lexbuf.len) then None
  else begin
    let ret = lexbuf.buf.(lexbuf.pos) in
    let char_at_boundary = Bitv.get lexbuf.bounds lexbuf.pos in
    lexbuf.pos <- lexbuf.pos + 1;
    if char_at_boundary && List.mem ret new_line_chars then new_line lexbuf;
    Some ret
  end

let mark_unsafe lexbuf i =
  lexbuf.marked_pos <- lexbuf.pos;
  lexbuf.marked_bol <- lexbuf.curr_bol;
  lexbuf.marked_line <- lexbuf.curr_line;
  lexbuf.marked_val <- i

let mark lexbuf i =
  let pos = lexbuf.pos in
  let start_pos = lexbuf.start_pos in
  (* Only allow marking after boundary points *)
  if start_pos = pos || (start_pos < pos && Bitv.get lexbuf.bounds (pos - 1)) then
    mark_unsafe lexbuf i

let start buf =
  buf.start_pos <- buf.pos;
  buf.start_bol <- buf.curr_bol;
  buf.start_line <- buf.curr_line;
  mark buf (-1)

let backtrack buf =
  buf.pos <- buf.marked_pos;
  buf.curr_bol <- buf.marked_bol;
  buf.curr_line <- buf.marked_line;
  buf.marked_val

let lexeme_length lexbuf = lexbuf.pos - lexbuf.start_pos
let lexeme_locs lexbuf = (lexbuf.start_pos + lexbuf.offset, lexbuf.pos + lexbuf.offset)
let lexing_positions lexbuf =
  let start_p = {
    Lexing.pos_fname = lexbuf.filename;
    pos_lnum = lexbuf.start_line;
    pos_cnum = lexbuf.start_pos + lexbuf.offset;
    pos_bol = lexbuf.start_bol;
  } and curr_p = {
    Lexing.pos_fname = lexbuf.filename;
    pos_lnum = lexbuf.curr_line;
    pos_cnum = lexbuf.pos + lexbuf.offset;
    pos_bol = lexbuf.curr_bol;
  } in
  (start_p, curr_p)

let lexeme_cp lexbuf =
  Array.sub lexbuf.buf (lexbuf.start_pos) (lexbuf.pos - lexbuf.start_pos)

let lexeme_cp_segments lexbuf =
  let rev_segment : Uchar.t list ref = ref [] in
  let rev_segments : (Uchar.t array) list ref = ref [] in
  for i = lexbuf.start_pos to (lexbuf.pos - 1) do
    let ch = Array.get lexbuf.buf i in
    let is_last = Bitv.get lexbuf.bounds i in
    rev_segment := ch :: !rev_segment;
    if is_last then
      let segment = List.rev !rev_segment |> Array.of_list in
      rev_segment := [];
      rev_segments := segment :: !rev_segments
  done;
  List.rev !rev_segments

let lexeme_cp_segments_arr lexbuf =
  lexeme_cp_segments lexbuf
  |> Array.of_list


let lexeme_utf_8 lexbuf =
  let bytes = Buffer.create 42 in
  for i = lexbuf.start_pos to (lexbuf.pos - 1) do
    let ch = Array.get lexbuf.buf i in
    Uutf.Buffer.add_utf_8 bytes ch
  done;
  Buffer.contents bytes

let utf_8_segments (b: Bitv.t) (arr: Uchar.t array) ~(start:int) ~(end_:int) =
  let bytes_buf = Buffer.create 42 in
  let rev_segments = ref [] in
  for i = start to end_ do
    let ch = Array.get arr i in
    let is_last = Bitv.get b i in
    let () = Uutf.Buffer.add_utf_8 bytes_buf ch in
    if is_last then
      let segment = Buffer.contents bytes_buf in
      let () = Buffer.clear bytes_buf in
      rev_segments := segment :: !rev_segments
  done;
  List.rev !rev_segments

let lexeme_utf_8_segments lexbuf =
  utf_8_segments lexbuf.bounds lexbuf.buf ~start:lexbuf.start_pos ~end_:(lexbuf.pos - lexbuf.start_pos)

let lexeme_utf_8_segments_arr lexbuf =
  lexeme_utf_8_segments lexbuf
  |> Array.of_list

(* Default *)
let lexeme = lexeme_utf_8_segments_arr

module Code_point = struct
  let lexeme = lexeme_cp
  let lexeme_segments = lexeme_cp_segments
  let lexeme_segments_arr = lexeme_cp_segments_arr
end

module Utf_8 = struct
  let lexeme = lexeme_utf_8
  let lexeme_segments = lexeme_utf_8_segments
  let lexeme_segments_arr = lexeme_utf_8_segments_arr
end
