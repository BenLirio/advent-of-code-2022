type expr =
  | List of expr list
  | Int of int
type expr_pair = expr * expr
type problem = expr_pair list