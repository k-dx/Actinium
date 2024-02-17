type t = VBool of bool | VInt of int | VString of string | VNil

val string_of_val : t -> string