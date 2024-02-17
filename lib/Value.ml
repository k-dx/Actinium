type t = VBool of bool | VInt of int | VString of string | VNil

let string_of_val (v : t) : string =
  match v with
  | VBool b -> string_of_bool b
  | VInt n -> string_of_int n
  | VString s -> s
  | VNil -> "nil"