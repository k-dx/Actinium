module Env : sig
  type 'a t
end

type env
and env_tok_val
and env_val = env * value
and value

val fresh_env : env
val string_of_value : value -> string
val interpret_stmts : env -> Parser.stmt list -> (env_val, string) Err.t
val interpret : Parser.stmt list -> (value, string) Err.t
