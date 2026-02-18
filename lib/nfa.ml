open Base

type state =
  | Compare of char * state
  | Split of state * state
  | Cycle of state ref
  | Finish

let sexp_of_state state =
  let seen = ref [] in
  let id = ref 0 in
  let rec go = function
    | Finish -> Sexp.Atom "Finish"
    | Compare (c, next) -> Sexp.List [ Atom "Compare"; Atom (String.make 1 c); go next ]
    | Split (l, r) -> Sexp.List [ Atom "Split"; go l; go r ]
    | Cycle r ->
      (match List.Assoc.find !seen ~equal:phys_equal r with
       | Some name -> Sexp.Atom (Printf.sprintf "<back:%s>" name)
       | None ->
         let name = Printf.sprintf "s%d" !id in
         Int.incr id;
         seen := (r, name) :: !seen;
         Sexp.List [ Atom (Printf.sprintf "<label:%s>" name); go !r ])
  in
  go state
;;

let rec compile = function
  | Parser.Atom c -> fun next -> Compare (c, next)
  | Parser.Concat (l, r) ->
    let l' = compile l in
    let r' = compile r in
    fun next -> l' (r' next)
  | Parser.Alt (l, r) ->
    let l' = compile l in
    let r' = compile r in
    fun next -> Split (l' next, r' next)
  | Parser.ZeroOrOne e ->
    let e' = compile e in
    fun next -> Split (e' next, next)
  | Parser.ZeroOrMore e ->
    let e' = compile e in
    fun next ->
      let r = ref Finish in
      let s = Split (e' (Cycle r), next) in
      r := s;
      s
  | Parser.OneOrMore e ->
    let e' = compile e in
    fun next ->
      let r = ref Finish in
      let s = e' (Split (Cycle r, next)) in
      r := s;
      s
;;

let build ast = (compile ast) Finish
