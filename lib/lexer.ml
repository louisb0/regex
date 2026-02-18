open Base

exception Unexpected of Token.t

type t =
  { raw : string
  ; pos : int
  }

let init raw = { raw; pos = 0 }
let advance t = { t with pos = t.pos + 1 }

let peek t =
  if t.pos >= String.length t.raw then Token.EOF else Token.of_char (String.get t.raw t.pos)
;;

let consume t expected =
  let ch = peek t in
  if Token.equal ch expected then advance t else raise (Unexpected ch)
;;
