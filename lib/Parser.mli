type expr =
  | Literal of Value.t
  | ArrayLiteral of expr list
  | Variable of Scanner.token
  | ArrayIndex of expr * expr (* expr[expr] = indexee[index] *)
  | Assignment of Scanner.token * expr
  | ArrayAssignment of
      Scanner.token * expr list * expr (* identifier * indexes * value  *)
  | Function of Scanner.token list * stmt (* parameters * body *)
  | Call of expr * expr list (* callee * arguments *)
  | Unary of Scanner.token_kind * expr
  | Binary of expr * Scanner.token_kind * expr
  | Logical of expr * Scanner.token_kind * expr
  | Grouping of expr

and stmt =
  | Expr of expr
  | Print of expr
  | Let of Scanner.token * expr option
  | Block of stmt list
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Return of expr

val parse : Scanner.token list -> (stmt list, string list) Err.t
