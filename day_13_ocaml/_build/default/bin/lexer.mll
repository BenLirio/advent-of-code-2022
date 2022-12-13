{
  open Lexing
  open Parser
}

rule token = parse
  | [' ' '\t' '\n' ] { token lexbuf }
  | [ '0'-'9' ]+ as lxm { INT (int_of_string lxm) }
  | [ ',' ] { COMMA }
  | [ '[' ] { LBRAC }
  | [ ']' ] { RBRAC }
  | eof { EOF }