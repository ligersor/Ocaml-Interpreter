(* Author:         Alex MacCauley
 * Major:          Computer Sceince
 * Creation Date:  3/27/2026
 * Due Date:       March 4/13/2026
 * Course:         CPSC 310 010
 * Professor Name: Dr. Schwesinger
 * Assignment:     #4
 * Filename:       lexer.ml
 * Purpose:        Lexer for c like language *)

open Token

(*
 * Function Name: tokenize
 * Description: Turns a string into a list of tokens For a parser 
 * Parameters: string input: the function that is to be lexxed
 * Return Value: token list: returns a list of tokens for the parser
 *)
let tokenize (input : string) : token list =
  (*Regular expressions to identify tokens*)
  let re_whitespace = Str.regexp "[ \n\t]" in
  let re_int = Str.regexp "-?[0-9]+" in
  let re_id = Str.regexp "[A-Za-z_][A-Za-z0-9_]*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let re_lbrace = Str.regexp "{" in
  let re_rbrace = Str.regexp "}" in
  let re_add = Str.regexp "\\+" in
  let re_sub = Str.regexp "-" in
  let re_mult = Str.regexp "\\*" in
  let re_div = Str.regexp "/" in
  let re_eq = Str.regexp "==" in
  let re_notequal = Str.regexp "!=" in
  let re_less = Str.regexp "<" in
  let re_greater = Str.regexp ">" in
  let re_lessequal = Str.regexp "<=" in
  let re_greaterequal = Str.regexp ">=" in
  let re_assign = Str.regexp "=" in
  let re_or = Str.regexp "||" in
  let re_and = Str.regexp "&&" in
  let re_not = Str.regexp "!" in
  let re_semi = Str.regexp ";" in
  let re_pow = Str.regexp "\\^" in

  let result = ref [] in
  let len = String.length input in
  let pos = ref 0 in

  (*keeps pointer on current position in string and attempts
   *to find the longest matching token from current
   *position. Ends when end of the string is reached*)
  while !pos < len do
    if Str.string_match re_whitespace input !pos then
      incr pos
    else if Str.string_match re_id input !pos then
      let token = Str.matched_string input in
      let tok =
        match token with
        | "print" -> Tok_Print
        | "int" -> Tok_Int_Type
        | "bool" -> Tok_Bool_Type
        | "true" -> Tok_Bool(true)
        | "false" -> Tok_Bool(false)
        | "if" -> Tok_If
        | "else" -> Tok_Else
        | "for" -> Tok_For
        | "from" -> Tok_From
        | "to" -> Tok_To
        | "while" -> Tok_While
        | _ -> Tok_ID(token)
      in
      result := tok :: !result;
      pos := !pos + String.length token
    else if Str.string_match re_int input !pos then
      let token = Str.matched_string input in
      result := Tok_Int(int_of_string token) :: !result;
      pos := !pos + (String.length token)
    else if Str.string_match re_sub input !pos then
            (result := Tok_Sub :: !result; incr pos)
    else if Str.string_match re_lparen input !pos then
            (result := Tok_LParen :: !result; incr pos)
    else if Str.string_match re_rparen input !pos then
            (result := Tok_RParen :: !result; incr pos)
    else if Str.string_match re_add input !pos then
            (result := Tok_Add :: !result; incr pos)
    else if Str.string_match re_mult input !pos then
            (result := Tok_Mult :: !result; incr pos)
    else if Str.string_match re_eq input !pos then
            (result := Tok_Equal :: !result; pos := !pos + String.length (Str.matched_string input))
    else if Str.string_match re_div input !pos then
            (result := Tok_Div :: !result; incr pos)
    else if Str.string_match re_notequal input !pos then
            (result := Tok_NotEqual  :: !result; pos:= !pos + String.length (Str.matched_string input))
    else if Str.string_match re_not input !pos then
            (result := Tok_Not :: !result ; incr pos)
    else if Str.string_match re_or input !pos then
            (result := Tok_Or :: !result; pos := !pos + String.length (Str.matched_string input))
    else if Str.string_match re_and input !pos then
            (result := Tok_And :: !result; pos := !pos + String.length (Str.matched_string input))
    else if Str.string_match re_assign input !pos then
            (result := Tok_Assign :: !result; incr pos)
    else if Str.string_match re_lessequal input !pos then
            (result := Tok_LessEqual :: !result; pos := !pos + String.length (Str.matched_string input))
    else if Str.string_match re_greaterequal input !pos then
            (result := Tok_GreaterEqual :: !result;pos := !pos + String.length (Str.matched_string input))
    else if Str.string_match re_less input !pos then
            (result := Tok_Less :: !result; incr pos)
    else if Str.string_match re_greater input !pos then
            (result := Tok_Greater :: !result; incr pos)
    else if Str.string_match re_semi input !pos then
            (result := Tok_Semi :: !result; incr pos)
    else if Str.string_match re_pow input !pos then
            (result := Tok_Pow :: !result; incr pos)
    else if Str.string_match re_lbrace input !pos then
            (result := Tok_LBrace :: !result; incr pos)
    else if Str.string_match re_rbrace input !pos then
            (result := Tok_RBrace :: !result; incr pos)
    else failwith (Printf.sprintf "unrecognized token - position %d: %s" (!pos) (String.sub input !pos 1))
  done;
  List.rev(EOF :: !result)
