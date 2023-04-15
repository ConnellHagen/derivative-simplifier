open Simplifier

let _ = add_file "../data/rules.ddx"

let _ = List.map print_endline (List.map showRule !rules)

(* let () = printExpr (Int 3) *)

(* let test_sub = Substitution.empty *)