open Base
open Regex

let test raw =
  let ast =
    match Parser.parse raw with
    | Ok ast -> ast
    | Error tok -> failwith (Sexp.to_string (Token.sexp_of_t tok))
  in
  let nfa = Nfa.build ast in
  Stdio.print_s (Nfa.sexp_of_state nfa)
;;

let%expect_test "char" =
  test "a";
  [%expect {| (Compare a Finish) |}]
;;

let%expect_test "concat" =
  test "ab";
  [%expect {| (Compare a (Compare b Finish)) |}]
;;

let%expect_test "alt" =
  test "a|b";
  [%expect {| (Split (Compare a Finish) (Compare b Finish)) |}]
;;

let%expect_test "zero or one" =
  test "a?";
  [%expect {| (Split (Compare a Finish) Finish) |}]
;;

let%expect_test "zero or more" =
  test "a*";
  [%expect {| (Split (Compare a (<label:s0> (Split (Compare a <back:s0>) Finish))) Finish) |}]
;;

let%expect_test "one or more" =
  test "a+";
  [%expect {| (Compare a (Split (<label:s0> (Compare a (Split <back:s0> Finish))) Finish)) |}]
;;

let%expect_test "quantifier literal" =
  test "a*b";
  [%expect
    {|
    (Split
     (Compare a (<label:s0> (Split (Compare a <back:s0>) (Compare b Finish))))
     (Compare b Finish))
    |}]
;;

let%expect_test "group quantifier" =
  test "(ab)*";
  [%expect
    {|
    (Split
     (Compare a
      (Compare b (<label:s0> (Split (Compare a (Compare b <back:s0>)) Finish))))
     Finish)
    |}]
;;

let%expect_test "alt quantifier" =
  test "(a|b)*";
  [%expect
    {|
    (Split
     (Split (Compare a <back:s0>)
      (Compare b
       (<label:s0>
        (Split (Split (Compare a <back:s0>) (Compare b <back:s0>)) Finish))))
     Finish)
    |}]
;;
