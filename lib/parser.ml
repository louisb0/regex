open Base

module Precedence = struct
  type t =
    | Floor
    | Infix
    | Grouping
    | Postfix
  [@@deriving compare, sexp]

  let of_token = function
    | Token.Bar -> Infix
    | Token.Star | Token.Plus | Token.QMark -> Postfix
    | Token.LParen | Token.Literal _ -> Grouping
    | Token.RParen | Token.EOF -> Floor
  ;;

  let binds token ~within = compare (of_token token) within > 0
end

type ast =
  | Atom of char
  | Concat of ast * ast
  | Alt of ast * ast
  | ZeroOrOne of ast
  | ZeroOrMore of ast
  | OneOrMore of ast
[@@deriving sexp]

let rec primary lex =
  match Lexer.peek lex with
  | Token.Literal c -> Lexer.advance lex, Atom c
  | Token.LParen ->
    let lex = Lexer.advance lex in
    let lex, expr = expression lex ~within:Precedence.Floor in
    let lex = Lexer.consume lex Token.RParen in
    lex, expr
  | tok -> raise (Lexer.Unexpected tok)

and step lex ~lhs =
  match Lexer.peek lex with
  | Token.Bar ->
    let lex = Lexer.advance lex in
    let lex, rhs = expression lex ~within:Precedence.Infix in
    lex, Alt (lhs, rhs)
  | Token.QMark -> Lexer.advance lex, ZeroOrOne lhs
  | Token.Star -> Lexer.advance lex, ZeroOrMore lhs
  | Token.Plus -> Lexer.advance lex, OneOrMore lhs
  | Token.LParen | Token.Literal _ ->
    let lex, rhs = expression lex ~within:Precedence.Grouping in
    lex, Concat (lhs, rhs)
  | tok -> raise (Lexer.Unexpected tok)

and expression lex ~within =
  let rec climb ~within (lex, lhs) =
    if not (Precedence.binds (Lexer.peek lex) ~within)
    then lex, lhs
    else step lex ~lhs |> climb ~within
  in
  primary lex |> climb ~within
;;

let parse raw =
  match Lexer.init raw |> expression ~within:Precedence.Floor with
  | lex, ast ->
    (match Lexer.peek lex with
     | Token.EOF -> Ok ast
     | unexpected -> Error unexpected)
  | exception Lexer.Unexpected tok -> Error tok
;;
