
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | RBRAC
    | LBRAC
    | INT of (
# 4 "bin/parser.mly"
       (int)
# 17 "bin/parser.ml"
  )
    | EOF
    | COMMA
  
end

include MenhirBasics

# 1 "bin/parser.mly"
  
  open Ast

# 30 "bin/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_problem) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: problem. *)

  | MenhirState01 : (('s, _menhir_box_problem) _menhir_cell1_LBRAC, _menhir_box_problem) _menhir_state
    (** State 01.
        Stack shape : LBRAC.
        Start symbol: problem. *)

  | MenhirState07 : (('s, _menhir_box_problem) _menhir_cell1_expr, _menhir_box_problem) _menhir_state
    (** State 07.
        Stack shape : expr.
        Start symbol: problem. *)

  | MenhirState12 : (('s, _menhir_box_problem) _menhir_cell1_expr_pair, _menhir_box_problem) _menhir_state
    (** State 12.
        Stack shape : expr_pair.
        Start symbol: problem. *)

  | MenhirState14 : (('s, _menhir_box_problem) _menhir_cell1_expr, _menhir_box_problem) _menhir_state
    (** State 14.
        Stack shape : expr.
        Start symbol: problem. *)


and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_expr_pair = 
  | MenhirCell1_expr_pair of 's * ('s, 'r) _menhir_state * (Ast.expr_pair)

and ('s, 'r) _menhir_cell1_LBRAC = 
  | MenhirCell1_LBRAC of 's * ('s, 'r) _menhir_state

and _menhir_box_problem = 
  | MenhirBox_problem of (Ast.problem) [@@unboxed]

let _menhir_action_01 =
  fun xs ->
    let cs = 
# 229 "<standard.mly>"
    ( xs )
# 76 "bin/parser.ml"
     in
    (
# 16 "bin/parser.mly"
                                               ( List(cs) )
# 81 "bin/parser.ml"
     : (Ast.expr))

let _menhir_action_02 =
  fun v ->
    (
# 17 "bin/parser.mly"
          ( Int(v) )
# 89 "bin/parser.ml"
     : (Ast.expr))

let _menhir_action_03 =
  fun e1 e2 ->
    (
# 19 "bin/parser.mly"
                    ( (e1, e2) )
# 97 "bin/parser.ml"
     : (Ast.expr_pair))

let _menhir_action_04 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 105 "bin/parser.ml"
     : (Ast.problem))

let _menhir_action_05 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 113 "bin/parser.ml"
     : (Ast.problem))

let _menhir_action_06 =
  fun () ->
    (
# 139 "<standard.mly>"
    ( [] )
# 121 "bin/parser.ml"
     : (Ast.expr list))

let _menhir_action_07 =
  fun x ->
    (
# 141 "<standard.mly>"
    ( x )
# 129 "bin/parser.ml"
     : (Ast.expr list))

let _menhir_action_08 =
  fun es ->
    (
# 21 "bin/parser.mly"
                           ( es )
# 137 "bin/parser.ml"
     : (Ast.problem))

let _menhir_action_09 =
  fun x ->
    (
# 238 "<standard.mly>"
    ( [ x ] )
# 145 "bin/parser.ml"
     : (Ast.expr list))

let _menhir_action_10 =
  fun x xs ->
    (
# 240 "<standard.mly>"
    ( x :: xs )
# 153 "bin/parser.ml"
     : (Ast.expr list))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | COMMA ->
        "COMMA"
    | EOF ->
        "EOF"
    | INT _ ->
        "INT"
    | LBRAC ->
        "LBRAC"
    | RBRAC ->
        "RBRAC"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_10 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_problem =
    fun _menhir_stack _v ->
      let es = _v in
      let _v = _menhir_action_08 es in
      MenhirBox_problem _v
  
  let rec _menhir_run_13 : type  ttv_stack. (ttv_stack, _menhir_box_problem) _menhir_cell1_expr_pair -> _ -> _menhir_box_problem =
    fun _menhir_stack _v ->
      let MenhirCell1_expr_pair (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_05 x xs in
      _menhir_goto_list_expr_pair_ _menhir_stack _v _menhir_s
  
  and _menhir_goto_list_expr_pair_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_problem) _menhir_state -> _menhir_box_problem =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState12 ->
          _menhir_run_13 _menhir_stack _v
      | MenhirState00 ->
          _menhir_run_10 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_problem) _menhir_state -> _menhir_box_problem =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LBRAC (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRAC ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let v = _v in
          let _v = _menhir_action_02 v in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState01 _tok
      | RBRAC ->
          let _v = _menhir_action_06 () in
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_problem) _menhir_state -> _ -> _menhir_box_problem =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRAC ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState07
          | INT _v_0 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let v = _v_0 in
              let _v = _menhir_action_02 v in
              _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState07 _tok
          | _ ->
              _eRR ())
      | RBRAC ->
          let x = _v in
          let _v = _menhir_action_09 x in
          _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_COMMA_expr_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_problem) _menhir_state -> _menhir_box_problem =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState07 ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState01 ->
          _menhir_run_03_spec_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_08 : type  ttv_stack. (ttv_stack, _menhir_box_problem) _menhir_cell1_expr -> _ -> _ -> _ -> _menhir_box_problem =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_10 x xs in
      _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_03_spec_01 : type  ttv_stack. (ttv_stack, _menhir_box_problem) _menhir_cell1_LBRAC -> _ -> _ -> _ -> _menhir_box_problem =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let x = _v in
      let _v = _menhir_action_07 x in
      _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v
  
  and _menhir_run_04 : type  ttv_stack. (ttv_stack, _menhir_box_problem) _menhir_cell1_LBRAC -> _ -> _ -> _ -> _menhir_box_problem =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_LBRAC (_menhir_stack, _menhir_s) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_01 xs in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_problem) _menhir_state -> _ -> _menhir_box_problem =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState14 ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState00 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState12 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState07 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState01 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_15 : type  ttv_stack. (ttv_stack, _menhir_box_problem) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_problem =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, e1) = _menhir_stack in
      let e2 = _v in
      let _v = _menhir_action_03 e1 e2 in
      let _menhir_stack = MenhirCell1_expr_pair (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LBRAC ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | INT _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let v = _v_0 in
          let _v = _menhir_action_02 v in
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState12 _tok
      | EOF ->
          let _v = _menhir_action_04 () in
          _menhir_run_13 _menhir_stack _v
      | _ ->
          _eRR ()
  
  and _menhir_run_14 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_problem) _menhir_state -> _ -> _menhir_box_problem =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | LBRAC ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState14
      | INT _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let v = _v_0 in
          let _v = _menhir_action_02 v in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_problem =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRAC ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let v = _v in
          let _v = _menhir_action_02 v in
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | EOF ->
          let _v = _menhir_action_04 () in
          _menhir_run_10 _menhir_stack _v
      | _ ->
          _eRR ()
  
end

let problem =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_problem v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
