(** This module is roughly equivalent to the module Sedlexing of sedlex, except
   that its lexbuffers handle Unicode code points marked with whether they are a
   final code point before a boundary or not. *)

type lybuf

(** {1 Creating a lexer buffer} *)

val create :
  ?filename:string -> [`LastUchar of Uchar.t | `Uchar of Uchar.t] Gen.t -> lybuf
(** [create ?filename gen] creates a lexer buffer, with the optionally provided
   [filename] used for outputing lexing positions. When the lexer needs
   characters, it will call [gen] for more characters. If the generator returns
   [`LastUchar c] it means that the code point [c] is a final code point before
   a boundary. *)

(** {1 Interface for lexer semantic actions} *)

val start : lybuf -> unit
(** [start buf] sets the internal slot of the buffer to [-1] and sets the
   [lexeme_start] position and backtrack position to the current position. *)

val next : lybuf -> Uchar.t option
(** [next buf] extracts the next code point form the lexer buffer and increments
   to the current position. If the input stream is exhausted it returns [None].
   If the following are encounted at a final code point position, the tracked
   line number in incremented.
   - Line Feed (U+000A)
   - Vertical Tab (U+000B)
   - Form Feed (U+000C)
   - Carriage Return (U+000D)
   - Next Line (U+0085)
   - Line Separator (U+2028)
   - Paragraph Separator (U+2029) *)

val mark : lybuf -> int -> unit
(** If the current code point position is a final position [mark buf i] stores
   the integer [i] in the interal slot and sets the backtrack position to the
   current position. If the current code point position is not, [mark buf i]
   does nothing. *)

val backtrack : lybuf -> int
(** [backtrack buf] returns the value stored in the internal slot of the buffer,
   and set the current position to the backtrack position. *)

(** {1 Interface for lexer semantic actions} *)

val lexeme_length : lybuf -> int
(** [lexeme_length buf] returns the number of code points in the lexeme. *)

val lexeme_locs : lybuf -> int*int
(** [lexeme_length buf] returns the code point locations of the lexeme. *)

val lexing_positions : lybuf -> Lexing.position*Lexing.position
(** [lexing_positions buf] returns the lexing positions of the lexeme. *)

(** {2 Lexemes} *)

(** We offer several kinds of lexemes. Some lexeme functions segment the lememe
   by code point boundaries. *)

(** If you are unsure about which kind of lexeme work best for your use case,
   the following (which is the same as [Utf_8.lexeme_segments_arr]) is a good
   default. *)

val lexeme : lybuf -> string array
(** [lexeme buf] returns the lexeme as an array of utf_8 segments. *)

(** {3 Code point lexemes} *)

module Code_point : sig

  val lexeme : lybuf -> Uchar.t array
  (** [lexeme buf] returns the lexeme as an array of code points. *)

  (** The following functions return array segments where a segment is an array
     of [Uchar.t],*)

  val lexeme_segments : lybuf -> Uchar.t array list
  (** [lexeme_segments buf] returns the lexeme as a list of array segments. *)

  val lexeme_segments_arr : lybuf -> Uchar.t array array
  (** [lexeme_segments_arr buf] returns the lexeme as an array of array
     segments. *)

end

(** {3 Utf_8 string lexemes} *)

module Utf_8 : sig

  val lexeme : lybuf -> string
  (** [lexeme buf] returns the lexeme as a string of utf_8 encoded bytes. *)

  (** The following functions return utf_8 segments where a segment is a string
     of utf_8 encoded bytes. *)

  val lexeme_segments : lybuf -> string list
  (** [lexeme_segments buf] returns the lexeme as a list of utf_8 segments. *)

  val lexeme_segments_arr : lybuf -> string array
  (** [lexeme_segments_arr buf] returns the lexeme as an array of utf_8
     segments. *)

end
