open Base
open Regex

let () =
  let output =
    match Parser.parse "ab*|(a)" with
    | Error t -> "unexpected: " ^ Sexp.to_string_hum (Token.sexp_of_t t)
    | Ok ast -> Sexp.to_string_hum (Nfa.sexp_of_state (Nfa.build ast))
  in
  Stdio.print_endline output
;;
