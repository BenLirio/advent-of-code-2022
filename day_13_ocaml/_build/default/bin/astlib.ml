let rec string_of_expr = function
  | Ast.Int v -> string_of_int v
  | Ast.List es -> "[" ^ (String.concat "," (List.map string_of_expr es)) ^ "]"
let string_of_expr_pair = function
  | (e1, e2) -> string_of_expr e1 ^ " < " ^ string_of_expr e2
let string_of_problem expr_pairs =
  String.concat "\n" (List.map string_of_expr_pair expr_pairs)
