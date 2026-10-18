type data_type =
  | Int_Type
  | Bool_Type

type expr =
  | Int of int
  | Bool of bool
  | ID of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Pow of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Greater of expr * expr
  | Less of expr * expr
  | GreaterEqual of expr * expr
  | LessEqual of expr * expr
  | Equal of expr * expr
  | NotEqual of expr * expr

type stmt =
  | NoOp
  | Seq of stmt * stmt
  | Declare of data_type * string
  | Assign of string * expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of string * expr * expr * stmt
  | Print of expr

let rec string_of_expr = function
  | Int n -> "Int(" ^ string_of_int n ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | ID s -> "ID(" ^ s ^ ")"
  | Add (a, b) -> "Add(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  | Sub (a, b) -> "Sub(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  | Mult (a, b) -> "Mult(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  | Div (a, b) -> "Div(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  | Pow (a, b) -> "Pow(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  | And (a, b) -> "And(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  | Or (a, b) -> "Or(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  | Not a -> "Not(" ^ string_of_expr a ^ ")"
  | Greater (a, b) -> "Greater(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  | Less (a, b) -> "Less(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  | GreaterEqual (a, b) -> "GreaterEqual(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  | LessEqual (a, b) -> "LessEqual(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  | Equal (a, b) -> "Equal(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"
  | NotEqual (a, b) -> "NotEqual(" ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ")"

let string_of_data_type = function
  | Int_Type -> "Int_Type"
  | Bool_Type -> "Bool_Type"

let rec string_of_stmt = function
  | NoOp -> "NoOp"
  | Seq (a, b) -> "Seq(" ^ string_of_stmt a ^ ", " ^ string_of_stmt b ^ ")"
  | Declare (t, s) -> "Declare(" ^ string_of_data_type t ^ ", " ^ s ^ ")"
  | Assign (s, e) -> "Assign(" ^ s ^ ", " ^ string_of_expr e ^ ")"
  | If (e, a, b) -> "If(" ^ string_of_expr e ^ ", " ^ string_of_stmt a ^ ", " ^ string_of_stmt b ^ ")"
  | While (e, s) -> "While(" ^ string_of_expr e ^ ", " ^ string_of_stmt s ^ ")"
  | For (v, a, b, s) -> "For(" ^ v ^ ", " ^ string_of_expr a ^ ", " ^ string_of_expr b ^ ", " ^ string_of_stmt s ^ ")"
  | Print e -> "Print(" ^ string_of_expr e ^ ")"
