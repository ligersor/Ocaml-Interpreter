(* 
 * Author:          Alex MacCauley 
 * Major:           Computer Science
 * Creation Date:   4/25
 * Due Date:        4/27
 * Course:          CPSC310 10
 * Professor Name:  Dr. Schwesinger
 * Assignment:      #5 
 * Filename:        eval.ml
 * purpose:         An interpreter for a C like language.
 *                  Takes a parse tree and produces the 
 *                  result of the code 
 *)
open Ast


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

type value = Int_Val of int | Bool_Val of bool
type environment = (string * value) list


(*
 * Function name: lookup
 * Description: searches for a given variable in the given environment
 * Parameters: env: the environment to be searched
               x: the variable to search for
 * Return Value: Some v if x is found or None if x is not found
 *)
let rec lookup env x =
  match env with
  | [] -> None
  | (y, v) :: rest ->
      if x = y then Some v else lookup rest x

(*
 * Function name: update
 * Description: searches environment for variable and updates its value
 * Parameters: env: the environment
 *             x: the variable
 *             v: the new value
 * Return Value: if x is not found the returns an error
 *               if x is found then returns the updated environment
 *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError "variable not found")
  | (y, _) :: rest when x = y -> (x, v) :: rest
  | pair :: rest -> pair :: update rest x v


let (print_buffer : Buffer.t) = Buffer.create 100

(*
 * Function name: print_output_string
 * Description: adds string to buffer and prints it
 * Parameters: s : the string
 * Return Value: unit
 *)
let print_output_string (s : string) : unit =
  Buffer.add_string print_buffer s;
  Stdlib.print_string s

(*
 * Function name: print_output_int
 * Description: converts int to string and uses it as the input for print_output_string
 * Parameters: i: the int
 * Return Value: unit
 *)
let print_output_int (i : int) : unit = print_output_string (string_of_int i)

(*
 * Function name: print_output_bool
 * Description: converts bool to string and uses it as the input for print_output_string
 * Parameters: b: the boolean
 * Return Value: unit
 *)
let print_output_bool (b : bool) : unit = print_output_string (string_of_bool b)

(*
 * Function name: print_output_newline
 * Description: uses newline as input for print_output_string
 * Parameters: Unit
 * Return Value: Unit
 *)
let print_output_newline () : unit = print_output_string "\n"

(*
 * Function name: eval_expr
 * Description: Evaluates an expression in the provided environment and returns its value
 * Parameters: env: the environment
               exp: the expression
 * Return Value: The value from evaluating the expression
 *)
let rec eval_expr (env : environment) (exp : expr) : value =
  match exp with 
    | Int x -> Int_Val x
    | Bool x -> Bool_Val x
    | ID x -> 
        (match lookup env x with
         | Some v -> v
         | None -> raise (DeclareError "undeclared variable"))
    (*Arithmetic Operators*)
    | Add (exp1,exp2) ->
        (match eval_expr env exp1, eval_expr env exp2 with
         | Int_Val v1, Int_Val v2 -> Int_Val (v1 + v2)
         | _ -> raise (TypeError "Addition can only be done with integers"))
    | Sub (exp1,exp2) ->
       (match eval_expr env exp1, eval_expr env exp2 with
        | Int_Val v1, Int_Val v2 -> Int_Val (v1 - v2)
        | _ -> raise (TypeError "Subtraction can only be done with integers"))
    | Mult (exp1,exp2) ->
       (match eval_expr env exp1, eval_expr env exp2 with
        | Int_Val v1, Int_Val v2 -> Int_Val (v1 * v2)
        | _ -> raise (TypeError "Multiplication can only be done with integers"))
    | Div (exp1,exp2) ->
       (match eval_expr env exp1, eval_expr env exp2 with
        | Int_Val _ , Int_Val 0 -> raise DivByZeroError
        | Int_Val v1, Int_Val v2 -> Int_Val (v1 / v2)
        | _ -> raise (TypeError "Division can only be done with integers"))
    | Pow (exp1,exp2) ->
       (match eval_expr env exp1, eval_expr env exp2 with
        | Int_Val v1, Int_Val v2 -> 
            let result =
              int_of_float (floor (float_of_int v1 ** float_of_int v2))
            in
            Int_Val result
        | _ -> raise (TypeError "Exponentiation can only be done with integers"))
    
    (*Boolean Operators*)
    | And (exp1,exp2) ->
        (match eval_expr env exp1, eval_expr env exp2 with
         | Bool_Val v1, Bool_Val v2 -> Bool_Val(v1 && v2) 
         | _ -> raise (TypeError "And operator can only be applied to two boolean expressions"))
    | Or (exp1,exp2) ->
        (match eval_expr env exp1, eval_expr env exp2 with
         | Bool_Val v1, Bool_Val v2 -> Bool_Val(v1 || v2)
         | _ -> raise (TypeError "Or operator can only be applied to two boolean expressions"))
    
    | Not exp ->
          (match eval_expr env exp with
           | Bool_Val x -> Bool_Val(not x)
           | _ -> raise (TypeError "! can only be applied to a boolen expression"))

    (*Relational Operators*)
    |Greater (exp1, exp2) ->
       (match eval_expr env exp1, eval_expr env exp2 with
        | Int_Val v1, Int_Val v2 -> Bool_Val (v1 > v2)
        | _ -> raise (TypeError "Relational operators require two integers"))
    |Less (exp1, exp2) ->
       (match eval_expr env exp1, eval_expr env exp2 with
        | Int_Val v1, Int_Val v2 -> Bool_Val (v1 < v2)
        | _ -> raise (TypeError "Relational operators require two integers"))
    |GreaterEqual (exp1, exp2) ->
       (match eval_expr env exp1, eval_expr env exp2 with
        | Int_Val v1, Int_Val v2 -> Bool_Val (v1 >= v2)
        | _ -> raise (TypeError "Relational operators require two integers"))
    |LessEqual (exp1, exp2) ->
       (match eval_expr env exp1, eval_expr env exp2 with
        | Int_Val v1, Int_Val v2 -> Bool_Val (v1 <= v2)
        | _ -> raise (TypeError "Relational operators require two integers"))

    (*Equality Operators*)
    |Equal (exp1,exp2) ->
       (match eval_expr env exp1, eval_expr env exp2 with
        | Int_Val v1, Int_Val v2 -> Bool_Val (v1 = v2)
        | Bool_Val v1, Bool_Val v2 -> Bool_Val (v1 = v2)
        | _ -> raise (TypeError "Expression types dont match"))
    |NotEqual (exp1,exp2) ->
       (match eval_expr env exp1, eval_expr env exp2 with
        | Int_Val v1, Int_Val v2 -> Bool_Val (v1 <> v2)
        | Bool_Val v1, Bool_Val v2 -> Bool_Val (v1 <> v2)
        | _ -> raise (TypeError "Expression types dont match"))

    
                
(*
 * Function name: eval_stmt
 * Description: returns the resulting environment of a statement in a given environment 
 * Parameters: env: the environment
               s: the statement
 * Return Value: the new environment
 *)
let rec eval_stmt env s =
  match s with 
    | NoOp -> env

    | Seq (s1 , s2) ->
        let env2 = eval_stmt env s1 in
        eval_stmt env2 s2

    | Declare (t, x) ->
        (match lookup env x with 
         | Some _ -> raise (DeclareError "Variable already exists")
         | None -> 
             let init =
               match t with
               | Int_Type -> Int_Val 0
               | Bool_Type -> Bool_Val false
             in (x, init) :: env)

    | Assign (x, e) ->
      (match lookup env x with
       | None -> raise (DeclareError "variable does not exist")
       | Some old_val ->
         let new_val = eval_expr env e in
         (match (old_val, new_val) with
          | Int_Val _, Int_Val _
          | Bool_Val _, Bool_Val _ ->
              update env x new_val
          | _ -> raise (TypeError "New value does not match variable type")))

    | If (cond, s_then, s_else) ->
      (match eval_expr env cond with
       | Bool_Val true -> eval_stmt env s_then
       | Bool_Val false -> eval_stmt env s_else
       | _ -> raise (TypeError "If statement expects boolean condition"))

    | While (cond, body) ->
      let rec loop env =
        match eval_expr env cond with
        | Bool_Val true ->
            let env' = eval_stmt env body in
            loop env'
        | Bool_Val false -> env
        | _ -> raise (TypeError "While loop expects boolean condition")
      in
      loop env
      
    | For (x, e_start, e_end, body) ->
      (match eval_expr env e_start, eval_expr env e_end with
       | Int_Val start_v, Int_Val end_v ->
           let rec loop env i =
             if i > end_v then env
             else
               let env' = update env x (Int_Val i) in
               let env'' = eval_stmt env' body in
               loop env'' (i + 1)
           in
           if start_v > end_v then env
           else loop env start_v
       | _ -> raise (TypeError "For loop start and end values must be integers"))

    | Print e ->
        let v = eval_expr env e in
        (match v with
         | Int_Val n -> print_output_int n
         | Bool_Val b -> print_output_bool b);
        print_output_newline ();
       env 

