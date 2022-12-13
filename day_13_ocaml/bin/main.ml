let _ =
  try
    let ic = open_in "input.txt" in
    let lexbuf = Lexing.from_channel ic in
    let problem = Parser.problem Lexer.token lexbuf in
    let solution = Solver.solve_problem problem in
    Printf.printf "solution: %s\n" (Solverlib.string_of_solution solution);
    Printf.printf "score: %d\n" (Solverlib.sum_solution_indexes solution);
  with End_of_file -> ()