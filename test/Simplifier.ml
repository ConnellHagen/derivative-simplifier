open Simplifier

(* Duplicate definitions because it doesn't work otherwise. 
   Update: it randomly started working again *)
(** The type of binary operators. *)
(* type bop = 
    | Add
    | Subt
    | Mult
    | Div
    | Pow
(** The type of the abstract syntax tree (AST). *)
type 'a expr =
    | Var of 'a
    | Fun of string * ('a expr) list
    | Int of int
    | Binop of bop * ('a expr) * ('a expr)
    | Ddx of 'a * ('a expr) *)


let _ = add_file "../data/rules.ddx"

(* let _ = List.map print_endline (List.map showRule !rules) *)

let test_sub = Substitution.empty

let () = Substitution.print_sub test_sub

(* x, y are different substitutions combined *)
let x_dif = Substitution.singleton "x" (Int 4)
let y_dif = Substitution.singleton "y" (Int 3)
let xy_dif = Substitution.combine_substitutions (Some x_dif) (Some y_dif)
let () = print_endline "Expected: x -> 4, y -> 3"
let () = Substitution.print_sub @@ Option.get xy_dif; print_endline ""

(* x, x are the same substitution combined *)
let x_same = Substitution.singleton "x" (Int 4)
let x_same2 = Substitution.singleton "x" (Int 4)
let xx2_same = Substitution.combine_substitutions (Some x_same) (Some x_same2)
let () = print_endline "Expected: x -> 4"
let () = Substitution.print_sub @@ Option.get xx2_same; print_endline ""

(* x, x are contradictory substitutions *)
let x_con = Substitution.singleton "x" (Int 4)
let x_con2 = Substitution.singleton "x" (Int 3)
let xx2_con = Substitution.combine_substitutions (Some x_con) (Some x_con2)
let () = print_endline "Expected: xx2_con = None"
let () = 
    match xx2_con with
    | None -> print_endline "`xx2_con` is `None`"; print_endline ""
    | Some _ -> Substitution.print_sub @@ Option.get xx2_con; print_endline ""

(* subsitute var with int *)
let replace_var = Substitution.singleton "x" (Int 3)
let (orig_var : string Simplifier__.Ast.expr) = (Var "x")
let () = print_endline "Expected: 3"
let () = printExpr (Substitution.substitute replace_var orig_var); print_endline ""

(* subsitute var with binop *)
let replace_binop = Substitution.singleton "x" (Binop (Add, Var "x", Var "y"))
let (orig_binop : string Simplifier__.Ast.expr) = (Var "x")
let () = print_endline "Expected: x+y"
let () = printExpr (Substitution.substitute replace_binop orig_binop); print_endline ""

(* substitute binop with int *)
let replace_bint = Substitution.singleton "x" (Int 3)
let (orig_bint : string Simplifier__.Ast.expr) = (Binop (Add, Var "x", Int 4))
let () = print_endline "Expected: 3+4"
let () = printExpr (Substitution.substitute replace_bint orig_bint); print_endline ""

(* substitute ddx with int *)
let replace_ddx = Substitution.singleton "x" (Var "y")
let (orig_ddx : string Simplifier__.Ast.expr) = (Ddx ("x", Binop (Mult, Int 2, Var "x")))
let () = print_endline "Expected: d/dy (2*y)"
let () = printExpr (Substitution.substitute replace_ddx orig_ddx); print_endline ""

(* substitute function with int *)
let replace_fun = Substitution.singleton "x" (Int 3)
let (orig_fun : string Simplifier__.Ast.expr) = Fun ("cos", [Binop (Add, Var "x", Int 90)])
let () = print_endline "Expected: cos(3+90)"
let () = printExpr (Substitution.substitute replace_fun orig_fun); print_endline ""

(* multiple substitutions in one *)
let sub_1 = Substitution.singleton "x" (Int 3)
let sub_2 = Substitution.singleton "y" (Int 5)
let replace_mult = Option.get @@ Substitution.combine_substitutions (Some sub_1) (Some sub_2)
let (orig_mult : string Simplifier__.Ast.expr) = Binop(Add, Binop(Mult, Var "x", Var "y"), Binop(Subt, Var "x", Var "y"))
let () = print_endline "Expected: (3*5)+(3-5)"
let () = printExpr (Substitution.substitute replace_mult orig_mult); print_endline ""
