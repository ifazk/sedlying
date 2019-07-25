(** Tests for [Sedlying] *)

let leg = "leg"
let legdots = "leg\u{0308}"

let mk_gen str = (* make a grapheme cluster gen *)
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String str) in
  let boundary = `Grapheme_cluster in
  Uugen.Uuseg_gen.of_decoder_replacing ~boundary ~decoder

let rleg = [%sedlex.regexp? "leg" ]
let rlegplus = [%sedlex.regexp? Plus "leg" ]
let rlegstar = [%sedlex.regexp? Star "leg" ]

module Sedlexing = Sedlying.Sedlexing

let mk_buf str = (* make a sedlying buffer with grapheme cluster boundaries *)
  Sedlexing.create (mk_gen str)

let test1 () =
  let buf = Sedlexing.create (mk_gen leg) in
  match%sedlex buf with
  | rleg -> ()
  | _ ->  failwith "test1 failed"

let legdots_list =
  let open Uchar in
  [ `LastUchar (of_char 'l')
  ; `LastUchar (of_char 'e')
  ; `Uchar (of_char 'g')
  ; `LastUchar (of_int 0x308)
  ]

let test2 () =
  let gen = mk_gen legdots in
  let list = Gen.to_list gen in
  if list = legdots_list then
    ()
  else
    failwith "test2 failed"

let test3 () =
  let buf = Sedlexing.create (mk_gen legdots) in
  match%sedlex buf with
  | rleg -> failwith "test3 failed, matched non_boundary"
  | eof -> failwith "test3 failed, mached eof"
  | _ -> ()

let test4 () =
  let buf = mk_buf (legdots ^ "leg") in
  match%sedlex buf with
  | rleg -> failwith "test4 failed, matched non_boundary"
  | eof -> failwith "test4 failed, mached eof"
  | _ -> ()

let test5 () =
  let buf = mk_buf ("legleg" ^ legdots) in
  match%sedlex buf with
  | rlegplus ->
    begin match Sedlexing.Utf_8.lexeme buf with
    | "legleg" -> ()
    | s -> failwith @@ "test5 failed, matched " ^ (Printf.sprintf "%S" s)
    end
  | eof -> failwith "test5 failed, mached eof"
  | _ -> ()

let test6 () =
  let buf = mk_buf (legdots ^ "legleg") in
  match%sedlex buf with
  | rlegstar ->
    begin match Sedlexing.Utf_8.lexeme buf with
    | "" -> ()
    | s -> failwith @@ "test6 failed, matched " ^ (Printf.sprintf "%S" s)
    end
  | eof -> failwith "test6 failed, mached eof"
  | _ -> ()

let test7 () =
  let buf = mk_buf ("legleg" ^ legdots) in
  match%sedlex buf with
  | rlegstar ->
    begin match Sedlexing.Utf_8.lexeme buf with
    | "legleg" -> ()
    | s -> failwith @@ "test7 failed, matched " ^ (Printf.sprintf "%S" s)
    end
  | eof -> failwith "test7 failed, mached eof"
  | _ -> ()

let () =
  ( test1 ()
  ; test2 ()
  ; test3 ()
  ; test4 ()
  ; test5 ()
  ; test6 ()
  ; test7 ()
  )
