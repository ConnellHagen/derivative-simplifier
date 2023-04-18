open Simplifier

let _ = add_file "../data/rules.ddx"

(* let _ = List.map print_endline (List.map showRule !rules) *)

let test_sub = Substitution.empty

let () = Substitution.print_sub test_sub

(* x, y are different substitutions combined *)
let x_dif = Substitution.singleton "x" (Int 4)
let y_dif = Substitution.singleton "y" (Int 3)
let xy_dif = Substitution.combine_substitutions (Some x_dif) (Some y_dif)
let () = Substitution.print_sub @@ Option.get xy_dif

(* x, x are the same substitution combined *)
let x_same = Substitution.singleton "x" (Int 4)
let x_same2 = Substitution.singleton "x" (Int 4)
let xx2_same = Substitution.combine_substitutions (Some x_same) (Some x_same2)
let () = Substitution.print_sub @@ Option.get xx2_same

(* x, x are contradictory substitutions *)
let x_con = Substitution.singleton "x" (Int 4)
let x_con2 = Substitution.singleton "x" (Int 3)
let xx2_con = Substitution.combine_substitutions (Some x_con) (Some x_con2)
let () = 
    match xx2_con with
    | None -> print_endline "`xx2_con is `None` (expected result)"
    | Some _ -> Substitution.print_sub @@ Option.get xx2_con

(* We are the group that cannot get these lines to work because something weird is happening *)
let t_exp = Int 3
let t_exp = Binop (Add, "x", "y")
PrintExpr @@ Substitution.substitute t_exp_sub t_exp