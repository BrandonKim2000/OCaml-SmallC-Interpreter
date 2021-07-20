open SmallCTypes
open EvalUtils
open TokenTypes

(* val eval_expr : SmallCTypes.environment -> SmallCTypes.expr -> SmallCTypes.value *)
(* val eval_stmt :SmallCTypes.environment -> SmallCTypes.stmt -> SmallCTypes.environment *)

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

(* Helper Functions *)

(* extend function, environment -> expression -> value -> (expression * value) list *)
(* Tuples the expression and the value together and appends it to the environment *)
let extend env x v = (x,v)::env
;;

(* search function, environment -> exxpression -> bool *)
(* Returns true if the expression is already in the environment *)
let rec search env x = 
  match env with 
  | [] -> false
  | (y,z)::t -> if x = y then true else (search t x)
  ;;

(* lookup function, environment -> expression -> value *)
(* Returns the value of the associated expression if it is in the environment *)
let rec lookup env x = 
  match env with 
  | [] -> failwith "no environment variables!"
  | (y,z)::t -> if x = y then z else (lookup t x)
  ;;

(* environment -> expr -> value *)
let rec eval_expr env t =
  match t with  
  | Int x -> Int_Val x
  | Bool x -> Bool_Val x
  | ID x -> if (search env x) = false 
            then raise (DeclareError("ID not found"))
            else (lookup env x) 
  | Add (e1, e2) -> let num1 = (eval_expr env e1) in 
                    let num2 = (eval_expr env e2) in 
                    (match num1 with
                    | Int_Val x -> (match num2 with 
                                   | Int_Val y-> (Int_Val (x + y))
                                   | Bool_Val y -> raise (TypeError("plus2 bool found")))
                    | Bool_Val x -> raise (TypeError("plus1 bool found")))
  | Sub (e1, e2) -> let num1 = (eval_expr env e1) in 
                    let num2 = (eval_expr env e2) in 
                    (match num1 with 
                    | Int_Val x -> (match num2 with 
                                    | Int_Val y -> (Int_Val (x-y))
                                    | Bool_Val y -> raise (TypeError("sub2 bool found")))
                    | Bool_Val x -> raise (TypeError("sub1 bool found")))
  | Mult (e1, e2) -> let num1 = (eval_expr env e1) in 
                     let num2 = (eval_expr env e2) in 
                     (match num1 with 
                     | Int_Val x -> (match num2 with 
                                    | Int_Val y -> (Int_Val (x*y))
                                    | Bool_Val y -> raise (TypeError("mult2 bool found")))
                     | Bool_Val x -> raise (TypeError("mult1 bool found")))
  | Div (e1, e2) -> let num1 = (eval_expr env e1) in 
                    let num2 = (eval_expr env e2) in 
                    (match num1 with 
                    | Int_Val x -> (match num2 with 
                                    | Int_Val y -> if y = 0 
                                    then raise (DivByZeroError) else (Int_Val (x/y))
                                    | Bool_Val y -> raise (TypeError("div2 bool found")))
                    | Bool_Val x -> raise (TypeError("div1 bool found")))
  | Pow (e1, e2) -> let num1 = (eval_expr env e1) in 
                    let num2 = (eval_expr env e2) in 
                    (match num1 with 
                    | Int_Val x -> (match num2 with 
                                    | Int_Val y -> 
                                    let z = ((float_of_int x)**(float_of_int y))
                                    in (Int_Val (int_of_float z))
                                    | Bool_Val y -> raise (TypeError("pow2 bool found")))
                    | Bool_Val x -> raise (TypeError("pow1 bool found")))
  | Or (e1, e2) -> let bool1 = (eval_expr env e1) in 
                   let bool2 = (eval_expr env e2) in 
                   (match bool1 with 
                   | Bool_Val x -> (match bool2 with 
                                    | Bool_Val y -> (Bool_Val (x || y))
                                    | Int_Val y -> raise (TypeError("or2 int found")))
                   | Int_Val x -> raise (TypeError("or1 int found")))
  | And (e1, e2) -> let bool1 = (eval_expr env e1) in 
                    let bool2 = (eval_expr env e2) in 
                    (match bool1 with 
                    | Bool_Val x -> (match bool2 with 
                                    | Bool_Val y -> (Bool_Val (x && y))
                                    | Int_Val y -> raise (TypeError("and2 int found")))
                    | Int_Val x -> raise (TypeError("and1 int found")))
  | Not exp -> let bool1 = (eval_expr env exp) in 
               (match bool1 with 
               | Bool_Val x-> (Bool_Val (not x))
               | Int_Val x -> raise (TypeError("not (func) int found")))
  | Greater (e1, e2) -> let num1 = (eval_expr env e1) in 
                        let num2 = (eval_expr env e2) in 
                        (match num1 with 
                        | Int_Val x -> (match num2 with 
                                        | Int_Val y -> (Bool_Val (x > y))
                                        | Bool_Val y -> raise (TypeError("greater2 bool found")))
                        | Bool_Val x -> raise (TypeError("greater1 bool found")))
  | Less (e1, e2) -> let num1 = (eval_expr env e1) in 
                     let num2 = (eval_expr env e2) in 
                     (match num1 with 
                     | Int_Val x -> (match num2 with 
                                    | Int_Val y -> (Bool_Val (x < y))
                                    | Bool_Val y -> raise (TypeError("less2 bool found")))
                     | Bool_Val x -> raise (TypeError("less1 bool found")))
  | GreaterEqual (e1, e2) -> let num1 = (eval_expr env e1) in 
                             let num2 = (eval_expr env e2) in 
                             (match num1 with 
                             | Int_Val x -> (match num2 with 
                                            | Int_Val y -> (Bool_Val (x >= y))
                                            | Bool_Val y -> raise (TypeError("greaterEqual2 bool found")))
                             | Bool_Val x -> raise (TypeError("greaterEqual1 bool found")))
  | LessEqual (e1, e2) -> let num1 = (eval_expr env e1) in 
                          let num2 = (eval_expr env e2) in 
                          (match num1 with 
                          | Int_Val x -> (match num2 with 
                                          | Int_Val y -> (Bool_Val (x <= y))
                                          | Bool_Val y -> raise (TypeError("lessEqual2 bool found")))
                          | Bool_Val x -> raise (TypeError("lessEqual1 bool found")))
  | Equal (e1, e2) -> let expr1 = (eval_expr env e1) in 
                      let expr2 = (eval_expr env e2) in 
                      (match expr1 with 
                      | Int_Val x -> (match expr2 with 
                                      | Int_Val y -> (Bool_Val (x = y))
                                      | Bool_Val y -> raise (TypeError("equal1 invalid expr found")))
                      | Bool_Val x -> (match expr2 with 
                                      | Bool_Val y -> (Bool_Val (x = y))
                                      | Int_Val y -> raise (TypeError("equal2 invalid expr found"))))
  | NotEqual (e1, e2) -> let expr1 = (eval_expr env e1) in 
                         let expr2 = (eval_expr env e2) in 
                         (match expr1 with 
                         | Int_Val x -> (match expr2 with 
                                        | Int_Val y -> (Bool_Val (x != y))
                                        | Bool_Val y -> raise (TypeError("notEqual1 invalid expr found")))
                         | Bool_Val x -> (match expr2 with 
                                        | Bool_Val y -> (Bool_Val (x != y))
                                        | Int_Val y -> raise (TypeError("notEqual2 invalid expr found"))))
;;

(* Helper functions *)
let return_int x = 
  match x with 
  | Int_Val i -> i 
  | _ -> raise (TypeError("int expected"))
  ;;

(* environment -> stmt -> environment *)
let rec eval_stmt env s =
  match s with 
  | NoOp -> env
  | Seq (s1, s2) -> let env1 = (eval_stmt env s1) in 
                    let env2 = (eval_stmt env1 s2) in 
                    env2
  | Declare (t, id) -> if ((search env id) = true) 
                       then raise (DeclareError("eval_stmt declare, id already exist"))
                       else (match t with 
                            | Int_Type -> (extend env id (Int_Val(0)))
                            | Bool_Type -> (extend env id (Bool_Val(false))))
  | Assign (id, expr) -> if (search env id) = false
                         then raise (DeclareError("eval_stmt assign, id not found"))
                         else (let id_type = (lookup env id) in 
                                let expr_type = (eval_expr env expr) in 
                                (match id_type with 
                                | Int_Val x -> (match expr_type with 
                                               | Int_Val y -> (extend env id expr_type)
                                               | Bool_Val y -> raise (TypeError("eval_stmt assign1, invalid match found")))
                                | Bool_Val x -> (match expr_type with 
                                               | Bool_Val y -> (extend env id expr_type)
                                               | Int_Val y -> raise (TypeError("eval_stmt assign2, invalid match found")))))
  | If (exp, s1, s2) -> let e2 = (eval_expr env exp) in 
                        (match e2 with 
                        | Bool_Val x -> let e3 = (if x = true 
                                                  then (eval_stmt env s1) 
                                                  else (eval_stmt env s2)) in e3
                        | Int_Val x -> raise (TypeError("if evals to bool")))
  | While (exp, s) -> let e2 = (eval_expr env exp) in 
                      (match e2 with 
                      | Bool_Val x -> (match x with 
                                      | true -> (eval_stmt (eval_stmt env s) (While (exp,s)))
                                      | false -> env)
                                      | Int_Val x -> raise (TypeError("invalid while")))
  | For (id, stexpr, endexpr, body) -> let start = (eval_expr env stexpr) in 
                                       let start = (return_int start) in 
                                       let ends = (eval_expr env endexpr) in 
                                       let ends = (return_int ends) in 
                                       let env = (eval_stmt env (Assign(id, Int start))) in 
                                       let rec for_helper env ends id stmt = 
                                         let env = (eval_stmt env body) in 
                                         let curr = (eval_expr env (ID id)) in 
                                         let curr = (return_int curr) in 
                                         if (curr < ends) then 
                                        (let env = (eval_stmt env (Assign(id, Int (curr + 1)))) in 
                                        (for_helper env ends id body))
                                        else (eval_stmt env (Assign(id, Int (curr + 1)))) in 
                                        (for_helper env ends id body)

  | Print (expr) -> let e1 = (eval_expr env expr) in
                    (match e1 with 
                    | Int_Val x -> (print_output_int x);(print_output_newline());env 
                    | Bool_Val x -> (print_output_bool x);(print_output_newline());env)
;;

