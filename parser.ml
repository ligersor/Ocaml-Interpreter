(* Author:         Alex MacCauley
 * Major:          Computer Sceince
 * Creation Date:  3/27/2026
 * Due Date:       March 4/13/2026
 * Course:         CPSC 310 010
 * Professor Name: Dr. Schwesinger
 * Assignment:     #4
 * Filename:       parser.ml
 * Purpose:        parser for c like language *)
open Ast
open Token

(* Parsing helpers (you do not need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr


(*
 * Function Name: string_of_list
 * Description: converts a list into a string
 * Parameters: (f: 'a -> string): a function that takes type a' and turns it into a string
 *             (l: 'a list): list of elements of type 'a
 * Return Value: string: returns a string made from the list of elemtns provided
 *)
let string_of_list (f : 'a -> string) (l : 'a list) : string
    =
  "["
  ^ (String.concat ", " @@ List.map f l)
  ^ "]"

(* 
 * Funciton Name: lookahead
 * Description:Return the next token in the token list, throwing an error if the list is
 *             empty
 * Parameter: (toks: token list): list of tokens 
 * Return Value: token : returns the next token in the list if toks is non-empty
 *                       if empty raises an exception "No more tokens" 
 *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h :: _ -> h

(*  
 * Function Name: match_token
 * Description: Matches the next token in the list, throwing an error if it doesn't match
 *              the given token 
 * Parameters: (toks: token list): list of tokens
 *             (tok: token): token to match with next token in list
 * Return Value: token list: if first token in list matches tok then 
 *                            returns token list without the value matched  
 *                            otherwise raies an exception
 *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
          (Printf.sprintf "Expected %s from input %s, got %s"
            (string_of_token tok)
            (string_of_list string_of_token toks)
            (string_of_token h)))


 (*
 * Function Name: parse_expr
 * Description: parses an expression from list of tokens created by lexer 
 *              and returns returns the remaining unparsed tokens and abstract syntax tree
 * Parameters: (toks: token list): list of tokens to be parsed
 * Return Value: expr_result: returns the remaining tokens and the syntax tree as a pair (tokens, tree)  
 *)
let parse_expr (toks : token list) : expr_result =
  (*Entry point for parser, immediately goes into parse_Or*)
  let rec parse_E (ts : token list) : expr_result  =
    match lookahead ts with
    | _ -> parse_Or ts
  (*Handles OR expressions*)
  and parse_Or (ts : token list) : expr_result=
    let (t1, a1) = parse_And ts in 
    match lookahead t1 with
    | Tok_Or ->
      let m1 = match_token t1 Tok_Or in
      let (t2, a2) = parse_Or m1 in
      (t2, Or(a1, a2))
    | _ -> (t1,a1)
  (*Handles AND expressions*)
  and parse_And (ts : token list) : expr_result= 
    let (t1, a1) = parse_Eq ts in
    match lookahead t1 with
    | Tok_And -> 
      let m1 = match_token t1 Tok_And in
      let (t2, a2) = parse_And m1 in
      (t2, And(a1, a2))
    | _ -> (t1, a1)
  (*Handles equality expressions*)
  and parse_Eq (ts : token list) : expr_result=
    let (t1,a1) = parse_Re ts in
    match lookahead t1 with
    | Tok_Equal ->
      let m1 = match_token t1 Tok_Equal in
      let (t2, a2) = parse_Eq m1 in
      (t2, Equal (a1, a2))
    | Tok_NotEqual ->
      let m1 = match_token t1 Tok_NotEqual in
      let (t2, a2) = parse_Eq m1 in
      (t2, NotEqual (a1, a2))
    | _ -> (t1,a1) 
  (*Handles relational expressions*)
  and parse_Re (ts : token list) : expr_result=
    let(t1, a1) = parse_A ts in
    match lookahead t1 with
    | Tok_Greater ->
      let m1 = match_token t1 Tok_Greater in
      let (t2, a2) = parse_Re m1 in
      (t2, Greater (a1, a2))
    | Tok_Less ->
      let m1 = match_token t1 Tok_Less in
      let (t2, a2) = parse_Re m1 in
      (t2, Less (a1, a2))
    | Tok_GreaterEqual ->
      let m1 = match_token t1 Tok_GreaterEqual in
      let (t2, a2) = parse_Re m1 in
      (t2, GreaterEqual (a1, a2))
    | Tok_LessEqual ->
      let m1 = match_token t1 Tok_LessEqual in
      let (t2, a2) = parse_Re m1 in
      (t2, LessEqual (a1, a2))
    | _ -> (t1, a1)
  (*Handles additive expressions*)
  and parse_A (ts : token list) : expr_result =
    let (t1, a1) = parse_M ts in
    match lookahead t1 with
    | Tok_Add -> 
      let m1 = match_token t1 Tok_Add in
      let (t2, a2) = parse_A m1 in
      (t2, Add (a1, a2))
    | Tok_Sub ->
      let m1 = match_token t1 Tok_Sub in
      let (t2, a2) = parse_A m1 in
      (t2, Sub (a1, a2))
    | _ -> (t1, a1)
  (*Handles multiplicitive expressions*)
  and parse_M (ts : token list) : expr_result =
    let (t1, a1) = parse_Pow ts in
    match lookahead t1 with
    | Tok_Mult -> 
      let m1 = match_token t1 Tok_Mult in
      let (t2, a2) = parse_M m1 in
      (t2, Mult (a1, a2))
    | Tok_Div ->
      let m1 = match_token t1 Tok_Div in
      let (t2, a2) = parse_M m1 in
      (t2, Div (a1, a2))
    | _ -> (t1, a1)
  (*Handles power expressions*)
  and parse_Pow (ts: token list) : expr_result =
    let (t1, a1) = parse_N ts in
    match lookahead t1 with
    | Tok_Pow ->
      let m1 = match_token t1 Tok_Pow in
      let (t2,a2) = parse_Pow m1 in
      (t2, Pow (a1, a2))
    | _ -> (t1, a1)
  (*Handles not expressions*)
  and parse_N (ts: token list) : expr_result =
    match lookahead ts with
    | Tok_Not ->
      let m1 = match_token ts Tok_Not in
      let (t2,a1) = parse_N m1 in
      (t2, Not a1)
    | _ -> parse_P ts
  (*Handles paretheses*)
  and parse_P (ts : token list) : expr_result =
    match lookahead ts with
    | Tok_Int(i) ->
      let m1 = match_token ts (Tok_Int i) in
      (m1, (Int i))
    | Tok_ID(s) ->
      let m1 = match_token ts (Tok_ID s) in
      (m1, ID s)
    | Tok_LParen ->
      let m1 = match_token ts Tok_LParen in
      let (t2, a1) = parse_E m1 in
      let n2 = match_token t2 Tok_RParen in
      (n2, a1)
    | _ -> raise (InvalidInputException "parse_P")
  in parse_E toks

 (*
  * Function Name: parse_stmt
  * Description: parses statements from list of tokens created by lexer
  *              and returns returns the remaining unparsed tokens and abstract syntax tree
  * Parameters: (toks: token list): list of tokens to be parsed
  * Return Value: expr_result: returns the remaining tokens and the syntax tree as a pair (tokens, tree)
  *)

let rec parse_stmt (toks : token list) : stmt_result =
  match lookahead toks with
  (*Handles Int and Bool type declarations*)
  | Tok_Int_Type | Tok_Bool_Type ->
    let (t1, typ) = 
      match lookahead toks with
      | Tok_Int_Type ->
        let t = match_token toks Tok_Int_Type in
        (t, Int_Type)
      | Tok_Bool_Type ->
        let t = match_token toks Tok_Bool_Type in
        (t, Bool_Type)
      | _ -> raise (InvalidInputException "Declaration")
    in
    (match lookahead t1 with
      | Tok_ID s ->
        let t2 = match_token t1 (Tok_ID s) in
        let t3 = match_token t2 Tok_Semi in
        let (t4, rest) = parse_stmt t3 in
        (t4, Seq(Declare(typ, s), rest))
      | _ -> raise (InvalidInputException "declare id"))
  (*Handles token IDs*)
  | Tok_ID s ->
    let t1 = match_token toks (Tok_ID s) in
    let t2 = match_token t1 Tok_Assign in
    let (t3, e) = parse_expr t2 in
    let t4 = match_token t3 Tok_Semi in
    let (t5, rest) = parse_stmt t4 in
    (t5, Seq(Assign(s, e), rest))
  (*Handles print tokens*)
  | Tok_Print ->
      let t1 = match_token toks Tok_Print in
      let t2 = match_token t1 Tok_LParen in
      let (t3, e) = parse_expr t2 in
      let t4 = match_token t3 Tok_RParen in
      let t5 = match_token t4 Tok_Semi in
      let (t6, rest) = parse_stmt t5 in
      (t6, Seq(Print e, rest))
  (*Handles if and else tokens*)
  | Tok_If ->
      let t1 = match_token toks Tok_If in
      let t2 = match_token t1 Tok_LParen in
      let (t3, cond) = parse_expr t2 in
      let t4 = match_token t3 Tok_RParen in
      let t5 = match_token t4 Tok_LBrace in
      let (t6, then_branch) = parse_stmt t5 in
      let t7 = match_token t6 Tok_RBrace in
     
      (match lookahead t7 with
       | Tok_Else ->
           let t8 = match_token t7 Tok_Else in
           let t9 = match_token t8 Tok_LBrace in
           let (t10, else_branch) = parse_stmt t9 in
           let t11 = match_token t10 Tok_RBrace in
           let (t12, rest) = parse_stmt t11 in
           (t12, Seq(If(cond, then_branch, else_branch), rest))
      | _ -> 
           let (t8, rest) = parse_stmt t7 in
           (t8, Seq(If(cond, then_branch, NoOp), rest)))
  (*Handle while tokens*)
  | Tok_While ->
      let t1 = match_token toks Tok_While in
      let t2 = match_token t1 Tok_LParen in
      let (t3, cond) = parse_expr t2 in
      let t4 = match_token t3 Tok_RParen in
      let t5 = match_token t4 Tok_LBrace in
      let (t6, body) = parse_stmt t5 in
      let t7 = match_token t6 Tok_RBrace in
      let (t8, rest) = parse_stmt t7 in
      (t8, Seq(While(cond, body), rest))
  (*Handles for and id tokens*)
  | Tok_For ->
      let t1 = match_token toks Tok_For in
      let t2 = match_token t1 Tok_LParen in

      let (t3, var) =
        match lookahead t2 with
        | Tok_ID s ->
            let t = match_token t2 (Tok_ID s) in
            (t, s)
        | _ -> raise (InvalidInputException "for id")
      in

      let t4 = match_token t3 Tok_From in
      let (t5, e1) = parse_expr t4 in
      let t6 = match_token t5 Tok_To in
      let (t7, e2) = parse_expr t6 in

      let t8 = match_token t7 Tok_RParen in
      let t9 = match_token t8 Tok_LBrace in
      let (t10, body) = parse_stmt t9 in
      let t11 = match_token t10 Tok_RBrace in
      let (t12, rest) = parse_stmt t11 in
      (t12, Seq(For(var, e1, e2, body), rest))
      
  | _ -> (toks, NoOp)

let parse_prog toks : stmt =
  let t, a = parse_stmt toks in 
  let _ = match_token t EOF in
  a
