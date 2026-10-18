
open Project5

let print_usage () =
  print_endline "USAGE: dune exec ./main.exe -- <filename>"

let string_of_list f lst =
  String.concat "\n" (List.map f lst)

let () =
  if Array.length Sys.argv <> 2 then
    print_usage ()
  else
    try
      let filename = Sys.argv.(1) in
      let ic = open_in filename in
      let input = In_channel.input_all ic in
      close_in ic;

      let tokens = Lexer.tokenize input in
      print_endline "=== TOKENS ===";
      print_endline (string_of_list Token.string_of_token tokens);

      let ast = Parser.parse_prog tokens in
      print_endline "\n=== AST ===";
      print_endline (Ast.string_of_stmt ast);

      Buffer.clear Eval.print_buffer;
      ignore (Eval.eval_stmt [] ast);

      print_endline "\n=== PROGRAM OUTPUT ===";
      print_string (Buffer.contents Eval.print_buffer)

    with
    | Failure message -> print_endline ("Error: " ^ message)
    | Eval.TypeError message ->
        print_endline ("Type error: " ^ message)
    | Eval.DeclareError message ->
        print_endline ("Declaration error: " ^ message)
    | Eval.DivByZeroError ->
        print_endline "Error: division by zero"
    | Token.InvalidInputException message ->
        print_endline ("Lexer error: " ^ message)
