type cmp = Lt | Eq | Gt

let rec cmp_expr expr_pair =
  let open Ast in
  match expr_pair with
  | Int a, Int b when a < b -> Lt
  | Int a, Int b when a = b -> Eq
  | Int a, Int b when a > b -> Gt
  | List [], List [] -> Eq
  | List [], List _ -> Lt
  | List _, List [] -> Gt
  | List (h1::t1), List (h2::t2) ->
    (match cmp_expr (h1, h2) with
    | Lt -> Lt
    | Eq -> cmp_expr (List t1, List t2)
    | Gt -> Gt)
  | List a, Int b -> cmp_expr (List a, List [Int b])
  | Int a, List b -> cmp_expr (List [Int a], List b)

let solve_expr_pair = function
  | (a, b) -> match cmp_expr (a, b) with
    | Lt -> true
    | Eq -> false
    | Gt -> false

let rec solve_problem = function
  | [] -> []
  | h::t -> solve_expr_pair h :: solve_problem t