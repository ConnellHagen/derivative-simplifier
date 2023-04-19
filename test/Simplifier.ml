open Simplifier

(** The type of binary operators. *)
type bop = 
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
    | Ddx of 'a * ('a expr)

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

(* subsitute var with int *)
let replace_var = Substitution.singleton "x" (Int 3)
let (orig_var : string Simplifier__.Ast.expr) = (Var "x")
let () = printExpr (Substitution.substitute replace_var orig_var)

(* subsitute var with binop *)
let replace_binop = Substitution.singleton "x" (Binop (Add, Var "x", Var "y"))
let (orig_binop : string Simplifier__.Ast.expr) = (Var "x")
let () = printExpr (Substitution.substitute replace_binop orig_binop)

(* substitute binop with int *)
let replace_bint = Substitution.singleton "x" (Int 3)
let (orig_bint : string Simplifier__.Ast.expr) = (Binop (Add, Var "x", Int 4))
let () = printExpr (Substitution.substitute replace_bint orig_bint)

(* substitute ddx with int *)
let replace_ddx = Substitution.singleton "x" (Int 3)
let (orig_ddx : string Simplifier__.Ast.expr) = (Ddx ("y", Var "x"))
let () = printExpr (Substitution.substitute replace_ddx orig_ddx)

(* substitute function with int *)
let replace_fun = Substitution.singleton "x" (Int 3)
let (orig_fun : string Simplifier__.Ast.expr) = Fun ("f", [Var "x"; Binop (Mult, Var "x", Var "x")])
let () = printExpr (Substitution.substitute replace_fun orig_fun)