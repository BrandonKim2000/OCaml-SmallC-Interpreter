open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)
(* token list -> token list * expr *)
(* Takes a list of tokens and returns an AST representing the expression corresponding
to the given tokens, along with the new, reduced list of tokens. *)
(* Exceptions: if the next tokens in the token list do not represent an expression, 
raise InvalidInputExpression *)
let rec parse_expr toks : expr_result =
  let (t, expr) = parse_Or toks in
    t, expr

(* Parse the Expr rule *)
and parse_Or toks = 
  let (t, a) = parse_And toks in 
  match lookahead t with 
  | Tok_Or -> let t' = match_token t Tok_Or in 
              let (t'', o) = parse_Or t' in 
              (t'', Or (a, o))
  | _ -> t, a

and parse_And toks = 
  let (t, eqe) = parse_Eqe toks in 
  match lookahead t with 
  | Tok_And -> let t' = match_token t Tok_And in 
               let (t'', a) = parse_And t' in 
               (t'', And (eqe, a))
  | _ -> t, eqe

and parse_Eqe toks = 
 let (t, rele) = parse_Rele toks in 
 match lookahead t with 
 | Tok_Equal -> let t' = match_token t Tok_Equal in 
                let (t'', eqe) = parse_Eqe t' in 
                (t'', Equal (rele, eqe))
 | Tok_NotEqual -> let t' = match_token t Tok_NotEqual in 
                   let (t'', eqe) = parse_Eqe t' in 
                   (t'', NotEqual (rele, eqe))
 | _ -> t, rele

and parse_Rele toks = 
let (t, adde) = parse_Adde toks in 
match lookahead t with 
| Tok_Less -> let t' = match_token t Tok_Less in 
              let (t'', rele) = parse_Rele t' in 
              (t'', Less (adde, rele))
| Tok_Greater -> let t' = match_token t Tok_Greater in 
                 let (t'', rele) = parse_Rele t' in 
                 (t'', Greater (adde, rele))
| Tok_LessEqual -> let t' = match_token t Tok_LessEqual in 
                   let (t'', rele) = parse_Rele t' in 
                  (t'', LessEqual (adde, rele))
| Tok_GreaterEqual -> let t' = match_token t Tok_GreaterEqual in 
                      let (t'', rele) = parse_Rele t' in 
                      (t'', GreaterEqual (adde, rele))
| _ -> t, adde

and parse_Adde toks = 
let (t, multe) = parse_Multe toks in 
match lookahead t with 
| Tok_Add -> let t' = match_token t Tok_Add in 
             let (t'', adde) = parse_Adde t' in 
             (t'', Add (multe, adde))
| Tok_Sub -> let t' = match_token t Tok_Sub in 
             let (t'', sube) = parse_Adde t' in 
             (t'', Sub (multe, sube))
| _ -> t, multe

and parse_Multe toks = 
let (t, powe) = parse_Powe toks in 
match lookahead t with 
| Tok_Mult -> let t' = match_token t Tok_Mult in 
              let (t'', multe) = parse_Multe t' in 
              (t'', Mult (powe, multe))
| Tok_Div -> let t' = match_token t Tok_Div in 
             let (t'', dive) = parse_Multe t' in 
             (t'', Div (powe, dive))
| _ -> t, powe

and parse_Powe toks = 
let (t, unarye) = parse_Unarye toks in 
match lookahead t with 
| Tok_Pow -> let t' = match_token t Tok_Pow in 
             let (t'', powe) = parse_Powe t' in 
             (t'', Pow (unarye, powe)) 
| _ -> t, unarye

and parse_Unarye toks = 
if lookahead toks = Tok_Not then 
  let toks = match_token toks Tok_Not in 
  let (toks, un) = parse_Unarye toks in 
  (toks, Not(un))
else 
  let (toks, prim) = parse_Primarye toks in (toks, prim)

and parse_Primarye toks = 
match lookahead toks with 
| Tok_Int i -> let t = match_token toks (Tok_Int i) in
               (t, Int i)
| Tok_Bool b -> let t = match_token toks (Tok_Bool b) in 
                (t, Bool b)
| Tok_ID s -> let t = match_token toks (Tok_ID s) in 
              (t, ID s)
| Tok_LParen -> let t = match_token toks Tok_LParen in 
                let (t', expr) = parse_expr t in 
                let t'' = match_token t' Tok_RParen in 
                (t'', expr)
| _ -> raise (InvalidInputException "parse_expr failed")
;;

(* token list -> token list * stmt *)
(* Takes a list of tokens and returns an AST representing the statement corresponding
to the given tokens, along with the new, reduced list of tokens *)
(* Exceptions: If the next tokens in the token list do not represent a statement, 
raise InvalidInputException *)
(* Match lookahead t with the keywords for each stmt, and then in parse_stmtop you 
actually do the parsing stuff *)
(* parse_stmt function *)
let rec parse_stmt toks = 
	match toks with

	| Tok_Int_Type::t -> 
		let (lst, dec) = (parse_declare toks) in
		let (lst2, stmt) = (parse_stmt lst) in
		(lst2, Seq(dec, stmt))

	| Tok_Bool_Type::t -> 
		let (lst, bool) = (parse_declare toks) in
		let (lst2, stmt) = (parse_stmt lst) in
		(lst2, Seq(bool, stmt))

	| Tok_ID s::t -> 
		let (lst, ass) = (parse_assign toks) in
		let (lst2, stmt) = (parse_stmt lst) in
		(lst2, Seq(ass, stmt))

	| Tok_Print::t -> 
		let (lst, print) = (parse_print toks) in
		let (lst2, stmt) = (parse_stmt lst) in
		(lst2, Seq(print, stmt))

	| Tok_If::t -> 
		let (lst, if_stmt) = (parse_if toks) in
		let (lst2, stmt) = (parse_stmt lst) in
		(lst2, Seq(if_stmt, stmt))

  | Tok_For::t -> 
    let (lst, for_stmt) = (parse_for toks) in 
    let (lst2, stmt) = (parse_stmt lst) in 
    (lst2, Seq(for_stmt, stmt))

	| Tok_While::t -> 
		let (lst, while_stmt) = (parse_while toks) in
		let (lst2, stmt) = (parse_stmt lst) in
    (lst2, Seq(while_stmt, stmt))
  
  | Tok_Int i::t -> raise (InvalidInputException "parse_stmt failed")

  | _ -> (toks, NoOp)

(* DeclareStatement *)
and parse_declare lst = match lst with
 
	| Tok_Int_Type::t -> let lst2 = (match_token lst Tok_Int_Type) in
		(match lst2 with
			| (Tok_ID s)::t ->   
				let lst3 = (match_token lst2 (Tok_ID s)) in
				let lst4 = (match_token lst3 Tok_Semi) in
				(lst4, Declare(Int_Type, s))
			| _ -> raise (InvalidInputException("declare error in int")))
	
	| Tok_Bool_Type::t -> let lst2 = (match_token lst Tok_Bool_Type) in
		(match lst2 with
			| (Tok_ID s)::t ->
				let lst3 = (match_token lst2 (Tok_ID s)) in
				let lst4 = (match_token lst3 Tok_Semi) in
				(lst4, Declare(Bool_Type, s))
			| _ -> raise (InvalidInputException("declare error in bool")))
	| _ -> raise (InvalidInputException("declare error"))
	

(* AssignStatement *)
and parse_assign lst = match lst with

	| (Tok_ID s)::t -> let lst2 = (match_token lst (Tok_ID s)) in
			let lst3 = (match_token lst2 Tok_Assign) in
			let (lst4, exp) = (parse_expr lst3) in 
			let lst5 = (match_token lst4 Tok_Semi) in
			(lst5, Assign(s, exp))
	| _ -> raise (InvalidInputException("assign error"))

		
(* Print Statement *)
and parse_print lst = match lst with
		
	| Tok_Print::t -> let lst2 = (match_token lst Tok_Print) in
			let lst3 = (match_token lst2 Tok_LParen) in
			let (lst4, exp) = (parse_expr lst3) in
			let lst5 = (match_token lst4 Tok_RParen) in 
			let lst6 = (match_token lst5 Tok_Semi) in
			(lst6, Print(exp))
	| _ -> raise (InvalidInputException("print error"))

	
(* IfStatement *)	
and parse_if lst = match lst with
			
	| Tok_If::t -> let lst2 = (match_token lst Tok_If) in
		(* if (expression) *)
		let lst3 = (match_token lst2 Tok_LParen) in
		let (lst4, if_exp) = (parse_expr lst3) in
		let lst5 = (match_token lst4 Tok_RParen) in
			
		(* {statement} *)
		let lst6 = (match_token lst5 Tok_LBrace) in
		let (lst7, if_stmt) = (parse_stmt lst6) in
		let lst8 = (match_token lst7 Tok_RBrace) in
			
		(* else or NoOp move *)
		(match lst8 with
		| Tok_Else::t -> let lst9 = (match_token lst8 Tok_Else) in
			let lst10 = (match_token lst9 Tok_LBrace) in
			let (lst11, else_stmt) = (parse_stmt lst10) in
			let lst12 = (match_token lst11 Tok_RBrace) in
			(lst12, If(if_exp, if_stmt, else_stmt))  
		| _ -> (lst8, If(if_exp, if_stmt, NoOp)))
	
	| _ -> raise (InvalidInputException("if error"))    


and parse_for lst = match lst with 

  | Tok_For::t -> let lst2 = (match_token lst Tok_For) in 
    let lst3 = (match_token lst2 Tok_LParen) in 
    (match lst3 with 
    | (Tok_ID s)::t -> 
      let lst4 = (match_token lst3 (Tok_ID s)) in 
      let lst5 = (match_token lst4 Tok_From) in 
      let (lst6, expr1) = (parse_expr lst5) in 
      let lst7 = (match_token lst6 Tok_To) in 
      let (lst8, expr2) = (parse_expr lst7) in 
      let lst9 = (match_token lst8 Tok_RParen) in 
      let lst10 = (match_token lst9 Tok_LBrace) in 
      let (lst11, stmt) = (parse_stmt lst10) in 
      let lst12 = (match_token lst11 Tok_RBrace) in 
      (lst12, For(s, expr1, expr2, stmt))
    | _ -> raise (InvalidInputException("for error after lparen match")))
  
  | _ -> raise (InvalidInputException("for error"))

(* WhileStatement *)
and parse_while lst = match lst with		
	
	| Tok_While::t -> let lst2 = (match_token lst Tok_While) in
		let lst3 = (match_token lst2 Tok_LParen) in
		let (lst4, exp) = (parse_expr lst3) in
		let lst5 = (match_token lst4 Tok_RParen) in
		let lst6 = (match_token lst5 Tok_LBrace) in
		let (lst7, stmt) = (parse_stmt lst6) in
		let lst8 = (match_token lst7 Tok_RBrace) in (lst8, While(exp, stmt))	
	| _ -> raise (InvalidInputException("while error"))
;;

let parse_main toks : stmt =
  let lst = (match_token toks Tok_Int_Type) in 
  let lst1 = (match_token lst Tok_Main) in 
  let lst2 = (match_token lst1 Tok_LParen) in 
  let lst3 = (match_token lst2 Tok_RParen) in 
  let lst4 = (match_token lst3 Tok_LBrace) in 

  let (lst4, stmt) = (parse_stmt lst4) in 
  let lst5 = (match_token lst4 Tok_RBrace) in 
  let lst6 = (match_token lst5 EOF) in stmt
  ;;