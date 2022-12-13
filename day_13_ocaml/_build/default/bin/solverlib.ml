let rec string_of_solution = function
  | [] -> ""
  | h::tl -> string_of_bool h ^ " " ^ string_of_solution tl

let sum_solution_indexes solution =
  let rec aux cur = function
    | [] -> 0
    | true::tl -> cur + aux (cur + 1) tl
    | false::tl -> aux (cur + 1) tl
  in aux 1 solution