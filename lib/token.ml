open Base

type t =
  | Literal of char
  | Bar
  | Star
  | Plus
  | QMark
  | LParen
  | RParen
  | EOF
[@@deriving sexp, equal]

let of_char = function
  | '|' -> Bar
  | '*' -> Star
  | '+' -> Plus
  | '?' -> QMark
  | '(' -> LParen
  | ')' -> RParen
  | c -> Literal c
;;
