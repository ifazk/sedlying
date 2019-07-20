(** Tests for [Sedlying] *)

let leg = "leg"
let legdots = "leg\u{0308}"

let leg_gen () =
  let decoder = Uutf.decoder (`String leg) in
  let boundary = `Grapheme_cluster in
  Uugen.Uuseg_gen.of_decoder_replacing ~boundary ~decoder
let legdots_gen () =
  let decoder = Uutf.decoder (`String legdots) in
  let boundary = `Grapheme_cluster in
  Uugen.Uuseg_gen.of_decoder_replacing ~boundary ~decoder

let rleg = [%sedlex.regexp? "leg" ]

module Sedlexing = Sedlying.Sedlexing


let test1 () =
  let buf = Sedlexing.create (leg_gen ()) in
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
  let gen = legdots_gen () in
  let list = Gen.to_list gen in
  if list = legdots_list then
    ()
  else
    failwith "test2 failed"

let test3 () =
  let buf = Sedlexing.create (legdots_gen ()) in
  match%sedlex buf with
  | rleg -> failwith "test3 failed, matched non_boundary"
  | eof -> failwith "test3 failed, mached eof"
  | _ -> ()


let () =
  ( test1 ()
  ; test2 ()
  ; test3 ()
  )
