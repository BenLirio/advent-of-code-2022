
(* The type of tokens. *)

type token = 
  | RBRAC
  | LBRAC
  | INT of (int)
  | EOF
  | COMMA

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val problem: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.problem)
