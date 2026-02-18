open Base
open Regex

let test raw =
  let result = Parser.parse raw in
  Stdio.print_s ([%sexp_of: (Parser.ast, Token.t) Result.t] result)
;;

let%expect_test "edge: empty string" =
  test "";
  [%expect {| (Error EOF) |}]
;;

let%expect_test "edge: empty group" =
  test "()";
  [%expect {| (Error RParen) |}]
;;

let%expect_test "operator: lone" =
  test "*";
  [%expect {| (Error Star) |}]
;;

let%expect_test "operator: double" =
  test "a**";
  [%expect {| (Ok (ZeroOrMore (ZeroOrMore (Atom a)))) |}]
;;

let%expect_test "operator: consecutive" =
  test "a*+";
  [%expect {| (Ok (OneOrMore (ZeroOrMore (Atom a)))) |}]
;;

let%expect_test "paren: unclosed" =
  test "(a*";
  [%expect {| (Error EOF) |}]
;;

let%expect_test "paren: unmatched" =
  test "a)";
  [%expect {| (Error RParen) |}]
;;

let%expect_test "alt: leading pipe" =
  test "|a";
  [%expect {| (Error Bar) |}]
;;

let%expect_test "alt: empty" =
  test "(|)";
  [%expect {| (Error Bar) |}]
;;

let%expect_test "alt: trailing" =
  test "(a|)";
  [%expect {| (Error RParen) |}]
;;

let%expect_test "precedence: concat vs alt" =
  test "ab|cd";
  [%expect {| (Ok (Alt (Concat (Atom a) (Atom b)) (Concat (Atom c) (Atom d)))) |}]
;;

let%expect_test "precedence: alt vs quantifier" =
  test "a|bc*";
  [%expect {| (Ok (Alt (Atom a) (Concat (Atom b) (ZeroOrMore (Atom c))))) |}]
;;

let%expect_test "combined" =
  test "((a*c|b)*|c+)?";
  [%expect
    {|
    (Ok
     (ZeroOrOne
      (Alt (ZeroOrMore (Alt (Concat (ZeroOrMore (Atom a)) (Atom c)) (Atom b)))
       (OneOrMore (Atom c)))))
    |}]
;;
