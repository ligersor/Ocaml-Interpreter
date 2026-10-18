(*
 * Author:          Alex MacCauley
 * Major:           Computer Science
 * Creation Date:   4/26
 * Due Date:        4/27
 * Course:          CPSC310 10
 * Professor Name:  Dr. Schwesinger
 * Assignment:      #5
 * Filename:        student.ml
 * purpose:         Unit tests for the interpreter
 *)
open Project5
open OUnit2
open Eval

let test_sanity _ = assert_equal 1 1

let test_1 _ = 
  Buffer.clear print_buffer;

  let input = "
  int x;
  int y;
  x = 5;
  y = 5;
  print (x + y); 
  " in

  let toks = Lexer.tokenize input in
  let t = Parser.parse_prog toks in 
  let _ = eval_stmt [] t in
  let output = Buffer.contents print_buffer in

  assert_equal "10\n" output ~msg: "addition"


let test_2 _ = 
  Buffer.clear print_buffer;

  let input = "
  int x;
  x = 1/0;
  " in
  let toks = Lexer.tokenize input in
  let t = Parser.parse_prog toks in

  assert_raises DivByZeroError (fun () -> 
    let _ = eval_stmt [] t in
    ()
  ) ~msg: "divByZero"

let test_3 _ =
  Buffer.clear print_buffer;
 
  let input = "
    int y;
    int i;
    y = 0;

    for(i from 0 to 5){
      y = y + 1;
    }
    print (y);
  "in
  let toks = Lexer.tokenize input in
  let t = Parser.parse_prog toks in
  let _ = eval_stmt [] t in
  let output = Buffer.contents print_buffer in

  assert_equal "6\n" output ~msg: "forLoop"

let test_4 _ =
  Buffer.clear print_buffer;

  let input = "
    int x;
    x = 0;
    while(x < 5){
       x = x + 1;
   }
   print(x);
  "in
  let toks = Lexer.tokenize input in
  let t = Parser.parse_prog toks in
  let _ = eval_stmt [] t in
  let output = Buffer.contents print_buffer in

  assert_equal "5\n" output ~msg: "whileLoop"

let test_5 _ =
  Buffer.clear print_buffer;

  let input = "
    int x;
    int y;
    x = 0;
    y = 1;
    print(x == y);
  "in
  let toks = Lexer.tokenize input in
  let t = Parser.parse_prog toks in
  let _ = eval_stmt [] t in
  let output = Buffer.contents print_buffer in

  assert_equal "false\n" output ~msg: "equality"

let test_6 _ =
  Buffer.clear print_buffer;

  let input = "
    int x;
    int y;
    x = 0;
    y = 1;
    print(x != y);
  "in
  let toks = Lexer.tokenize input in
  let t = Parser.parse_prog toks in
  let _ = eval_stmt [] t in
  let output = Buffer.contents print_buffer in

  assert_equal "true\n" output ~msg: "notEquality"

let test_7 _ =
  Buffer.clear print_buffer;

  let input = "
    int x;
    int y;
    x = 0;
    y = 1;
    print(!(x == y));
  "in
  let toks = Lexer.tokenize input in
  let t = Parser.parse_prog toks in
  let _ = eval_stmt [] t in
  let output = Buffer.contents print_buffer in

  assert_equal "true\n" output ~msg: "not(equality)"

let test_8 _ =
  Buffer.clear print_buffer;

  let input = "
    int x;
    int y;
    x = 0;
    y = 0;
    if(y == 0 && x == 0){
      print(x);
    }
  "in
  let toks = Lexer.tokenize input in
  let t = Parser.parse_prog toks in
  let _ = eval_stmt [] t in
  let output = Buffer.contents print_buffer in

  assert_equal "0\n" output ~msg: "if"

let test_9 _ =
  Buffer.clear print_buffer;

  let input = "
    int x;
    int y;
    int z;
    x = 0;
    y = 1;
    z = 2;
    x = z;
    x = y;
    z = x + z;
    print (z);
    
  "in
  let toks = Lexer.tokenize input in
  let t = Parser.parse_prog toks in
  let _ = eval_stmt [] t in
  let output = Buffer.contents print_buffer in

  assert_equal "3\n" output ~msg: "assign"

let test_10 _ =
  Buffer.clear print_buffer;

  let input = "
    int x;
    int y;
    int z;
    x = 5;
    y = 5;
    x = x * y;
    z = x / y;
    print(z);

  "in
  let toks = Lexer.tokenize input in
  let t = Parser.parse_prog toks in
  let _ = eval_stmt [] t in
  let output = Buffer.contents print_buffer in

  assert_equal "5\n" output ~msg: "multAndDiv"

let suite = 
  "student" >::: [ 
     "sanity" >:: test_sanity;
      "addition" >:: test_1;
      "divbyzero" >:: test_2;
      "forLoop" >:: test_3;
      "whileLoop" >:: test_4;
      "equality" >:: test_5;
      "notEquality" >:: test_6;
      "not(equality)" >:: test_7;
      "if" >:: test_8;
      "assign" >:: test_9;
      "multAndDiv" >:: test_10
 
 
 
   ]

let _ = run_test_tt_main suite
