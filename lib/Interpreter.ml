module Env = Map.Make (String)

type env = { parent_scope : env option; current_scope : value ref Env.t }
and env_tok_val = env * Scanner.token option * value
and env_val = env * value

and value =
  (* interpreter value *)
  | VBool of bool
  | VInt of int
  | VString of string
  | VNil
  | VArray of value list
  | VFunction of int * Scanner.token list * Parser.stmt * env
  | VNativeFunction of (value list -> value)

let rec string_of_value (value : value) : string =
  match value with
  | VBool b -> string_of_bool b
  | VInt n -> string_of_int n
  | VString s -> s
  | VNil -> "nil"
  | VArray elements ->
      "[" ^ String.concat ", " (List.map string_of_value elements) ^ "]"
  | VFunction (arity, _, _, _) -> "fn<" ^ string_of_int arity ^ ">"
  | VNativeFunction _ -> "builtin"

let is_truthy (v : value) : bool =
  match v with VNil -> false | VBool b when b = false -> false | _ -> true

let is_equal (v1 : value) (v2 : value) : bool =
  match (v1, v2) with
  | VFunction _, _ -> false
  | _, VFunction _ -> false
  | _ -> v1 = v2

let premade_env =
  [
    ("input", ref (VNativeFunction (fun _ -> VString (read_line ()))));
    ( "printf",
      ref
        (VNativeFunction
           (fun args ->
             let _ =
               List.map (fun x -> print_string (string_of_value x)) args
             in
             let _ = print_newline () in
             VNil)) );
  ]
  |> List.to_seq |> Env.of_seq

let mk_env parent_scope : env = { parent_scope; current_scope = Env.empty }

let fresh_env : env =
  {
    parent_scope = Some { parent_scope = None; current_scope = premade_env };
    current_scope = Env.empty;
  }

(* create a variable in the current scope *)
let define_val (env : env) (name : Scanner.token) (value : value) : env =
  { env with current_scope = Env.add name.lexeme (ref value) env.current_scope }

(* assign a value to an existing variable in any scope in env *)
let rec assign_val (env : env) (name : Scanner.token) (value : value) :
    (env, string) Err.t =
  let ( let* ) = Err.bind in
  match Env.find_opt name.lexeme env.current_scope with
  | Some v_ref ->
      v_ref := value;
      Err.return env
  | None -> (
      match env.parent_scope with
      | Some parent_env ->
          let* updated_parent_env = assign_val parent_env name value in
          Err.return
            {
              parent_scope = Some updated_parent_env;
              current_scope = env.current_scope;
            }
      | None -> Err.fail ("Undefined variable '" ^ name.lexeme ^ "'"))

let rec lookup_val (env : env) (name : Scanner.token) : (value, string) Err.t =
  match Env.find_opt name.lexeme env.current_scope with
  | Some v -> Err.return !v
  | None -> (
      match env.parent_scope with
      | Some env -> lookup_val env name
      | None ->
          Err.fail ("variable '" ^ name.lexeme ^ "' has not been declared!"))

let convert_value (v : Value.t) : value =
  match v with
  | VBool b -> VBool b
  | VInt n -> VInt n
  | VString s -> VString s
  | VNil -> VNil

let rec _print_env (env : env) =
  let _ = print_endline "scope --------------------------" in
  let _ =
    List.map
      (fun (name, value_ref) ->
        Printf.printf "%s = %s\n" name (string_of_value !value_ref))
      (Env.bindings env.current_scope)
  in
  match env.parent_scope with None -> () | Some env -> _print_env env
[@@ocaml.warning "-32"]

let rec multiply_list (n : int) (xs : 'a list) : 'a list =
  if n <= 0 then [] else xs @ multiply_list (n - 1) xs

exception Return of env * value

let replace l pos a =
  if pos >= List.length l then Err.fail "index out of bounds"
  else Err.return (List.mapi (fun i x -> if i = pos then a else x) l)

let rec replace_elems xs indexes value =
  let ( let* ) = Err.bind in
  match indexes with
  | [] -> Err.fail ""
  | i :: [] -> replace xs i value
  | i :: indexes -> (
      if i >= List.length xs then Err.fail "index out of bounds"
      else
        match List.nth xs i with
        | VArray inside_xs ->
            let* replaced = replace_elems inside_xs indexes value in
            replace xs i (VArray replaced)
        | _ -> Err.fail "cannot index non-array")

let rec eval_exprs (env : env) (exprs : Parser.expr list) :
    (env * value list, string) Err.t =
  let ( let* ) = Err.bind in
  List.fold_right
    (fun expr acc ->
      Err.bind acc (fun (env, evaluated_exprs) ->
          let* env, value = eval_expr env expr in
          Err.return (env, value :: evaluated_exprs)))
    exprs
    (Err.return (env, []))

and eval_expr (env : env) (expr : Parser.expr) : (env_val, string) Err.t =
  let ( let* ) = Err.bind in
  match expr with
  | Parser.Literal v -> Err.return (env, convert_value v)
  | Parser.ArrayLiteral elements ->
      let* env, evaluated_elements = eval_exprs env elements in
      Err.return (env, VArray evaluated_elements)
  | Parser.Variable name ->
      let* v = lookup_val env name in
      Err.return (env, v)
  | Parser.ArrayIndex (indexee, index) -> (
      let* env, evaluated_indexee = eval_expr env indexee in
      match evaluated_indexee with
      | VArray elements -> (
          let* env, evaluated_index = eval_expr env index in
          match evaluated_index with
          | VInt n ->
              if n < 0 then Err.fail "index of an array cannot be negative"
              else if n >= List.length elements then
                Err.fail "index out of bounds"
              else Err.return (env, List.nth elements n)
          | _ -> Err.fail "index of an array must be an int")
      | VString s -> (
          let* env, evaluated_index = eval_expr env index in
          match evaluated_index with
          | VInt n ->
              if n < 0 then Err.fail "index of a string cannot be negative"
              else if n >= String.length s then Err.fail "index out of bounds"
              else Err.return (env, VString (String.make 1 (String.get s n)))
          | _ -> Err.fail "index of a string must be an int")
      | _ -> Err.fail "trying to index non-array value")
  | Parser.Assignment (name, e) ->
      let* env, v = eval_expr env e in
      let* _ = assign_val env name v in
      Err.return (env, v)
  | Parser.ArrayAssignment (token, indexes, e) -> (
      let* _, evaluated_indexes = eval_exprs env indexes in
      let* _, new_value = eval_expr env e in
      let* current_value = lookup_val env token in
      match current_value with
      | VArray elements ->
          let* indexes =
            try
              Err.return
                (List.map
                   (fun evaluated_index ->
                     match evaluated_index with
                     | VInt n when n >= 0 -> n
                     | _ -> raise (Failure "index must be a non-negative int"))
                   evaluated_indexes)
            with Failure message -> Err.fail message
          in
          let* modified_elements = replace_elems elements indexes new_value in
          let modified_value = VArray modified_elements in
          let* _ = assign_val env token modified_value in
          Err.return (env, modified_value)
      | VString s -> (
          let* indexes =
            try
              Err.return
                (List.map
                   (fun evaluated_index ->
                     match evaluated_index with
                     | VInt n when n >= 0 -> n
                     | _ -> raise (Failure "index must be a non-negative int"))
                   evaluated_indexes)
            with Failure message -> Err.fail message
          in
          if List.length indexes = 0 then Err.fail "indexes cannot be empty"
          else if List.length indexes > 1 then
            Err.fail "strings are only one-dimensional, too many indexes"
          else
            match new_value with
            | VString s_char when String.length s_char = 1 ->
                let modified_string =
                  String.mapi
                    (fun i c ->
                      if i = List.hd indexes then String.get s_char 0 else c)
                    s
                in
                let modified_value = VString modified_string in
                let* _ = assign_val env token modified_value in
                Err.return (env, modified_value)
            | _ ->
                Err.fail
                  "only single-elements strings can be assigned to a string \
                   index")
      | _ ->
          Err.fail
            ("cannot do array assignment on non-array variable '"
           ^ token.Scanner.lexeme ^ "' ("
            ^ string_of_value current_value
            ^ ")"))
  | Parser.Function (parameters, body) ->
      Err.return (env, VFunction (List.length parameters, parameters, body, env))
  | Parser.Call (callee, arguments) -> (
      let* env, callee = eval_expr env callee in
      match callee with
      | VFunction (arity, parameters, body, fun_env) ->
          let* _, arguments =
            List.fold_right
              (fun arg_raw acc ->
                Err.bind acc (fun (env, args) ->
                    let* _, arg = eval_expr env arg_raw in
                    Err.return (env, arg :: args)))
              arguments
              (Err.return (env, []))
          in
          if arity = List.length arguments then
            let* fun_env =
              List.fold_right2
                (fun param arg_value acc_env ->
                  Err.bind acc_env (fun acc_env ->
                      let extended_env = define_val acc_env param arg_value in
                      Err.return extended_env))
                parameters arguments
                (Err.return (mk_env (Some fun_env)))
            in
            let* _, value =
              try interpret_stmt fun_env body
              with Return (env, value) -> Err.return (env, value)
            in
            Err.return (env, value)
          else
            Err.fail
              ("Expected " ^ string_of_int arity ^ " but got "
              ^ string_of_int (List.length arguments)
              ^ " for function '" ^ string_of_value callee ^ "'")
      | VNativeFunction f ->
          let* _, arguments =
            List.fold_right
              (fun arg_raw acc ->
                Err.bind acc (fun (env, args) ->
                    let* _, arg = eval_expr env arg_raw in
                    Err.return (env, arg :: args)))
              arguments
              (Err.return (env, []))
          in
          Err.return (env, f arguments)
      | _ ->
          Err.fail ("cannot call non-function '" ^ string_of_value callee ^ "'")
      )
  | Parser.Grouping e -> eval_expr env e
  | Parser.Unary (op, e) -> (
      let* env, v = eval_expr env e in
      match (op, v) with
      | Scanner.MINUS, VInt n -> Err.return (env, VInt (-n))
      | Scanner.MINUS, _ -> Err.fail "cannot use unary - on non-number value!"
      | Scanner.BANG, v -> Err.return (env, VBool (not (is_truthy v)))
      | _, _ -> Err.fail "unexpected unary operator!")
  | Parser.Logical (e1, op, e2) -> (
      let* env, v1 = eval_expr env e1 in
      match op with
      | Scanner.OR ->
          if is_truthy v1 then Err.return (env, v1) else eval_expr env e2
      | Scanner.AND ->
          if not (is_truthy v1) then Err.return (env, v1) else eval_expr env e2
      | _ -> Err.fail "unexpected logical operator!")
  | Parser.Binary (e1, op, e2) -> (
      let* env, v1 = eval_expr env e1 in
      let* env, v2 = eval_expr env e2 in
      match op with
      | Scanner.PLUS -> (
          match (v1, v2) with
          | VInt n1, VInt n2 -> Err.return (env, VInt (n1 + n2))
          | VString s1, VString s2 -> Err.return (env, VString (s1 ^ s2))
          | _, _ ->
              Err.fail
                "mismatched types for binary + operator (requires either two \
                 numbers or two strings)")
      | Scanner.MINUS -> (
          match (v1, v2) with
          | VInt n1, VInt n2 -> Err.return (env, VInt (n1 - n2))
          | _, _ ->
              Err.fail
                "mismatched types for binary - operator (requires two numbers)")
      | Scanner.STAR -> (
          match (v1, v2) with
          | VInt n1, VInt n2 -> Err.return (env, VInt (n1 * n2))
          | VInt n, VArray vs | VArray vs, VInt n ->
              if n < 0 then
                Err.fail "array can only be multiplied by non-negative number"
              else Err.return (env, VArray (multiply_list n vs))
          | _, _ ->
              Err.fail
                "mismatched types for binary - operator (requires two numbers)")
      | Scanner.SLASH -> (
          match (v1, v2) with
          | VInt n1, VInt n2 ->
              if n2 == 0 then Err.fail "division by zero!"
              else Err.return (env, VInt (n1 / n2))
          | _, _ -> Err.fail "mismatched types for binary - operator")
      | Scanner.GREATER -> (
          match (v1, v2) with
          | VInt n1, VInt n2 -> Err.return (env, VBool (n1 > n2))
          | _, _ ->
              Err.fail
                "mismatched types for binary > operator (requires two numbers)")
      | Scanner.GREATER_EQUAL -> (
          match (v1, v2) with
          | VInt n1, VInt n2 -> Err.return (env, VBool (n1 >= n2))
          | _, _ ->
              Err.fail
                "mismatched types for binary >= operator (requires two numbers)"
          )
      | Scanner.LESS -> (
          match (v1, v2) with
          | VInt n1, VInt n2 -> Err.return (env, VBool (n1 < n2))
          | _, _ ->
              Err.fail
                "mismatched types for binary < operator (requires two numbers)")
      | Scanner.LESS_EQUAL -> (
          match (v1, v2) with
          | VInt n1, VInt n2 -> Err.return (env, VBool (n1 <= n2))
          | _, _ ->
              Err.fail
                "mismatched types for binary <= operator (requires two numbers)"
          )
      | Scanner.EQUAL_EQUAL -> Err.return (env, VBool (is_equal v1 v2))
      | Scanner.BANG_EQUAL -> Err.return (env, VBool (not (is_equal v1 v2)))
      | _ -> Err.fail "invalid binary operation")

and interpret_stmt env (stmt : Parser.stmt) : (env_val, string) Err.t =
  let ( let* ) = Err.bind in
  match stmt with
  | Parser.Expr expr ->
      let* v = eval_expr env expr in
      Err.return v
  | Parser.Print expr ->
      let* env, v = eval_expr env expr in
      print_endline (string_of_value v);
      Err.return (env, VNil)
  | Parser.Let (name, init) ->
      let new_env = define_val env name VNil in
      let* _, value =
        match init with
        | Some expr -> eval_expr new_env expr
        | None -> Err.return (env, VNil)
      in
      let* env = assign_val new_env name value in
      Err.return (env, VNil)
  | Parser.Block stmts ->
      let block_env = mk_env (Some env) in
      let* _, v = interpret_stmts block_env stmts in
      Err.return (env, v)
  | Parser.If (expr, if_true, if_false) -> (
      let* env, condition = eval_expr env expr in
      if is_truthy condition then interpret_stmt env if_true
      else
        match if_false with
        | Some if_false -> interpret_stmt env if_false
        | None -> Err.return (env, VNil))
  | Parser.While (expr, stmt) ->
      let* env, condition = eval_expr env expr in
      if is_truthy condition then
        let* env, _ = interpret_stmt env stmt in
        interpret_stmt env (Parser.While (expr, stmt))
      else Err.return (env, VNil)
  | Parser.Return expr ->
      let* env, value = eval_expr env expr in
      raise (Return (env, value))

and interpret_stmts env (stmts : Parser.stmt list) : (env_val, string) Err.t =
  let ( let* ) = Err.bind in
  match stmts with
  | [] -> Err.return (env, VNil)
  | stmt :: [] -> interpret_stmt env stmt
  | stmt :: stmts ->
      let* env, _ = interpret_stmt env stmt in
      interpret_stmts env stmts

let interpret stmts =
  let ( let* ) = Err.bind in
  let* _, v = interpret_stmts fresh_env stmts in
  Err.return v
