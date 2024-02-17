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

let rec _string_of_expr (expr : expr) : string =
  match expr with
  | Literal v -> Value.string_of_val v
  | ArrayLiteral exprs ->
      "[" ^ String.concat "," (List.map _string_of_expr exprs) ^ "]"
  | Variable t -> t.lexeme
  | ArrayIndex (indexee, index) ->
      "(arrayIndex (" ^ _string_of_expr indexee ^ ") (" ^ _string_of_expr index
      ^ "))"
  | Assignment (t, e) -> t.lexeme ^ "=" ^ _string_of_expr e
  | ArrayAssignment (token, indexes, e) ->
      "(" ^ token.Scanner.lexeme ^ "["
      ^ String.concat "][" (List.map _string_of_expr indexes)
      ^ "] = " ^ _string_of_expr e ^ ")"
  | Function (parameters, _) ->
      "(fn ("
      ^ String.concat "), (" (List.map (fun p -> p.Scanner.lexeme) parameters)
      ^ ") <body> )"
  | Call (callee, arguments) ->
      "((" ^ _string_of_expr callee ^ ")" ^ "("
      ^ String.concat "), (" (List.map _string_of_expr arguments)
      ^ ")"
  | Unary (op, e) ->
      let op_str =
        match op with
        | MINUS -> "-"
        | BANG -> "!"
        | _ -> failwith "Illegal op in Unary"
      in
      "(" ^ op_str ^ " " ^ _string_of_expr e ^ ")"
  | Binary (e1, op, e2) | Logical (e1, op, e2) ->
      let op_str =
        match op with
        | PLUS -> "+"
        | MINUS -> "-"
        | SLASH -> "/"
        | STAR -> "*"
        | GREATER -> ">"
        | GREATER_EQUAL -> ">="
        | LESS -> "<"
        | LESS_EQUAL -> "<="
        | BANG_EQUAL -> "!="
        | EQUAL_EQUAL -> "=="
        | AND -> "and"
        | OR -> "or"
        | _ -> failwith "Illegal op in Binary"
      in
      "(" ^ op_str ^ " " ^ _string_of_expr e1 ^ " " ^ _string_of_expr e2 ^ ")"
  | Grouping e -> "(group " ^ _string_of_expr e ^ ")"
[@@ocaml.warning "-32"]

let _print_expr (expr : expr) = expr |> _string_of_expr |> print_endline
[@@ocaml.warning "-32"]

type parser = { tokens : Scanner.token list; current : int }

let mk_parser (tokens : Scanner.token list) : parser = { tokens; current = 0 }

type 'a result = (parser * 'a, parser * string) Err.t

let is_end (parser : parser) : bool =
  parser.current >= List.length parser.tokens

let get_token (parser : parser) : Scanner.token option =
  if is_end parser then None else Some (List.nth parser.tokens parser.current)

let previous (parser : parser) : Scanner.token =
  List.nth parser.tokens (parser.current - 1)

let match_token (parser : parser) (kind : Scanner.token_kind) : bool =
  match get_token parser with
  | Some t -> t.kind == kind
  | None -> kind == Scanner.EOF

let advance (parser : parser) : parser =
  { parser with current = parser.current + 1 }

let consume (parser : parser) (kind : Scanner.token_kind) (err_message : string)
    : Scanner.token result =
  match get_token parser with
  | Some t when t.kind = kind -> Err.return (advance parser, t)
  | Some t ->
      Err.fail
        ( parser,
          err_message ^ " (got '" ^ t.lexeme ^ "' on line "
          ^ string_of_int t.line ^ " instead)" )
  | None -> Err.fail (parser, err_message ^ " (got EOF instead)")

let rec synchronize (parser : parser) : parser =
  if
    (previous parser).kind == Scanner.SEMICOLON
    && not (match_token parser SEMICOLON)
  then parser
  else
    match get_token parser with
    | Some t
      when t.kind = Scanner.LET || t.kind = Scanner.FOR || t.kind = Scanner.IF
           || t.kind = Scanner.WHILE || t.kind = Scanner.PRINT
           || t.kind = Scanner.RETURN ->
        parser
    | Some _ -> synchronize (advance parser)
    | None -> parser

(* array -> "[]" | "[" expr ( "," expr )? "]" *)
let rec parse_array (parser : parser) : expr result =
  let ( let* ) = Err.bind in
  let* parser, _ =
    consume parser Scanner.LEFT_BRACKET
      "(parse_array): expected '[' as a beggining of an array"
  in
  let* parser, exprs =
    if match_token parser Scanner.RIGHT_BRACKET then Err.return (parser, [])
    else parse_expr_list parser
  in
  let* parser, _ =
    consume parser Scanner.RIGHT_BRACKET
      "(parse_array): expected ']' at the end of an array"
  in
  Err.return (parser, ArrayLiteral exprs)

(* primary        → NUMBER | STRING | "true" | "false" | "nil"
                  | "(" expression ")" | array ; *)
and parse_primary (parser : parser) : expr result =
  (* returns parser with current AFTER the last token in expr  *)
  let ( let* ) = Err.bind in
  match get_token parser with
  | Some t when t.kind = Scanner.NUMBER || t.kind = Scanner.STRING ->
      Err.return (advance parser, Literal (Option.get t.literal))
  | Some t when t.kind = Scanner.TRUE ->
      Err.return (advance parser, Literal (Value.VBool true))
  | Some t when t.kind = Scanner.FALSE ->
      Err.return (advance parser, Literal (Value.VBool false))
  | Some t when t.kind = Scanner.NIL ->
      Err.return (advance parser, Literal Value.VNil)
  | Some t when t.kind = Scanner.IDENTIFIER ->
      Err.return (advance parser, Variable t)
  | Some t when t.kind = Scanner.LEFT_PAREN -> (
      let* parser, e = parse_expr (advance parser) in
      match get_token parser with
      | Some t when t.kind = Scanner.RIGHT_PAREN ->
          Err.return (advance parser, Grouping e)
      | _ ->
          Err.fail
            ( parser,
              "parse_primary: unclosed parenthesis on line "
              ^ string_of_int t.line ^ ", got: '" ^ t.lexeme ^ "' instead!" ))
  | Some t when t.kind = Scanner.LEFT_BRACKET -> parse_array parser
  | Some t ->
      Err.fail
        ( parser,
          "parse_primary: unexpected token on line " ^ string_of_int t.line
          ^ ": '" ^ t.lexeme ^ "' when parsing primary expr!" )
  | None -> Err.fail (parser, "expected a token but got none")

(* exprList      → expr ( "," expr )* ; *)
(* === *)
(* exprList      → expr ( "," exprList )? ; *)
and parse_expr_list (parser : parser) : expr list result =
  let ( let* ) = Err.bind in
  let* parser, expr = parse_expr parser in
  if match_token parser Scanner.COMMA then
    let* parser, exprs = parse_expr_list (advance parser) in
    Err.return (parser, expr :: exprs)
  else Err.return (parser, [ expr ])

(* arrayIndex    -> primary ( "[" expr "]" )* ; *)
and parse_array_index (parser : parser) : expr result =
  let ( let* ) = Err.bind in
  let* parser, primary = parse_primary parser in
  let rec parse_index_helper (parser : parser) (indexee : expr) : expr result =
    if match_token parser Scanner.LEFT_BRACKET then
      let parser = advance parser in
      let* parser, index = parse_expr parser in
      let* parser, _ =
        consume parser Scanner.RIGHT_BRACKET
          "(parse_array_index): expected ']' after array index"
      in
      parse_index_helper parser (ArrayIndex (indexee, index))
    else Err.return (parser, indexee)
  in
  parse_index_helper parser primary

(* call           → arrayIndex ( "(" expr_list? ")" )* ; *)
and parse_call (parser : parser) : expr result =
  let ( let* ) = Err.bind in
  let* parser, callee = parse_array_index parser in
  let rec parse_arguments_helper (parser : parser) (callee : expr) : expr result
      =
    if match_token parser Scanner.LEFT_PAREN then
      let parser = advance parser in
      let* parser, arguments =
        if match_token parser Scanner.RIGHT_PAREN then Err.return (parser, [])
        else parse_expr_list parser
      in
      let* parser, _ =
        consume parser Scanner.RIGHT_PAREN
          "(parse_call): expected ')' after argument list"
      in
      parse_arguments_helper parser (Call (callee, arguments))
    else Err.return (parser, callee)
  in
  parse_arguments_helper parser callee

(* unary          → ( "!" | "-" ) unary | call ; *)
and parse_unary (parser : parser) : expr result =
  let ( let* ) = Err.bind in
  match get_token parser with
  | Some t when t.kind = Scanner.BANG || t.kind = Scanner.MINUS ->
      let* parser, e = parse_unary (advance parser) in
      Err.return (parser, Unary (t.kind, e))
  | _ -> parse_call parser

(* generalized function for parsing binary expressions *)
(* binary         → binary ( ( ops ) binary )* ; *)
and parse_binary next_precedence (ops : Scanner.token_kind list)
    (parser : parser) : expr result =
  let ( let* ) = Err.bind in
  let* parser, e1 = next_precedence parser in
  let token = get_token parser in
  match token with
  | Some t when List.exists (fun op -> op = t.kind) ops ->
      let* parser, e2 = parse_binary next_precedence ops (advance parser) in
      Err.return (parser, Binary (e1, t.kind, e2))
  | _ -> Err.return (parser, e1)

(* factor         → unary ( ( "/" | "*" ) unary )* ; *)
and parse_factor (parser : parser) : expr result =
  parse_binary parse_unary [ Scanner.SLASH; Scanner.STAR ] parser

(* term           → factor ( ( "-" | "+" ) factor )* ; *)
and parse_term (parser : parser) : expr result =
  parse_binary parse_factor [ Scanner.MINUS; Scanner.PLUS ] parser

(* comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ; *)
and parse_comparison (parser : parser) : expr result =
  (* ">" | ">=" | "<" | "<=" *)
  let ops =
    [ Scanner.GREATER; Scanner.GREATER_EQUAL; Scanner.LESS; Scanner.LESS_EQUAL ]
  in
  parse_binary parse_term ops parser

(* equality -> comparison ( ( "!=" | "==" ) comparison )*;
   ===
   equality -> comparison ( ( "!=" | "==" ) equality )? ;
*)
and parse_equality (parser : parser) : expr result =
  parse_binary parse_comparison
    [ Scanner.BANG_EQUAL; Scanner.EQUAL_EQUAL ]
    parser

(* generalized function to parse logic_or, logic_and *)
and parse_logical next_precedence (ops : Scanner.token_kind list)
    (parser : parser) : expr result =
  let ( let* ) = Err.bind in
  let* parser, e1 = next_precedence parser in
  let token = get_token parser in
  match token with
  | Some t when List.exists (fun op -> op = t.kind) ops ->
      let* parser, e2 = parse_binary next_precedence ops (advance parser) in
      Err.return (parser, Logical (e1, t.kind, e2))
  | _ -> Err.return (parser, e1)

(* logic_and      → equality ( "and" equality )* ; *)
(* === *)
(* logic_and      → equality ( "and" logic_and )? ; *)
and parse_and (parser : parser) : expr result =
  parse_logical parse_equality [ Scanner.AND ] parser

(* logic_or       → logic_and ( "or" logic_and )* ; *)
(* === *)
(* logic_or       → logic_and ( "or" logic_or )?  ; *)
and parse_or (parser : parser) : expr result =
  parse_logical parse_and [ Scanner.OR ] parser

(* parameters    -> IDENTIFIER ( "," parameters )? *)
and parse_parameters (parser : parser) : Scanner.token list result =
  let ( let* ) = Err.bind in
  let* parser, identifier =
    consume parser Scanner.IDENTIFIER
      "(parse_parameters): expected an identifier"
  in
  (* let identifier = Variable token in *)
  if match_token parser Scanner.COMMA then
    let* parser, parameters = parse_parameters (advance parser) in
    Err.return (parser, identifier :: parameters)
  else Err.return (parser, [ identifier ])

(* function      -> logic_or | FN "(" parameters? ")" block *)
and parse_function (parser : parser) : expr result =
  let ( let* ) = Err.bind in
  if match_token parser Scanner.FN then
    let parser = advance parser in
    let* parser, _ =
      consume parser Scanner.LEFT_PAREN
        "(parse_function): expected '(' after 'fn'"
    in
    let* parser, parameters =
      if match_token parser Scanner.RIGHT_PAREN then Err.return (parser, [])
      else parse_parameters parser
    in
    let* parser, _ =
      consume parser Scanner.RIGHT_PAREN
        "(parse_function): expected ')' after parameters list"
    in
    let* parser, body = parse_block parser in
    Err.return (parser, Function (parameters, body))
  else parse_or parser

(* assignment     → function
                    | IDENTIFIER ( "[" expression "]" )* "=" assignment ; *)
and parse_assignment (parser : parser) : expr result =
  let ( let* ) = Err.bind in
  let* parser, left_expr = parse_function parser in
  match get_token parser with
  | Some t when t.kind = EQUAL -> (
      let* parser, right_expr = parse_assignment (advance parser) in
      match left_expr with
      | Variable token -> Err.return (parser, Assignment (token, right_expr))
      | ArrayIndex (indexee, index) ->
          let rec unpack_array_index (e : expr) (indexes : expr list) =
            match e with
            | Variable token ->
                Err.return (parser, ArrayAssignment (token, indexes, right_expr))
            | ArrayIndex (indexee, index) ->
                unpack_array_index indexee (index :: indexes)
            | _ -> Err.fail (parser, "bad assignment to array index")
          in
          unpack_array_index (ArrayIndex (indexee, index)) []
      | _ ->
          Err.fail
            ( parser,
              "parse_assignment: line " ^ string_of_int t.line
              ^ ": cannot assign to non-variable and non-arrayindex l-value" ))
  | _ -> Err.return (parser, left_expr)

and parse_expr (parser : parser) : expr result = parse_assignment parser

(* printStmt -> "print" expr ";" *)
and parse_print (parser : parser) : stmt result =
  let ( let* ) = Err.bind in
  match get_token parser with
  | Some t when t.kind = Scanner.PRINT -> (
      match parse_expr (advance parser) with
      | Err.Ok (parser, expr) ->
          let* parser, _ =
            consume parser Scanner.SEMICOLON
              "expected ';' after variable print statement"
          in
          Err.return (parser, Print expr)
      | Err.Error (parser, message) -> Err.fail (parser, message))
  | Some t ->
      Err.fail
        ( parser,
          "(parse_print) line " ^ string_of_int t.line
          ^ ": expected print keyword, got " ^ t.lexeme ^ " instead" )
  | _ ->
      Err.fail (parser, "(parse_print) expected print keyword, got EOF instead")

(* exprStmt -> expr ";" *)
and parse_expr_stmt (parser : parser) : stmt result =
  match parse_expr parser with
  | Err.Error (parser, message) -> Err.fail (parser, message)
  | Err.Ok (parser, expr) -> (
      match get_token parser with
      | Some t when t.kind = Scanner.SEMICOLON ->
          Err.return (advance parser, Expr expr)
      | Some t ->
          Err.fail
            ( parser,
              "(parse_expr_stmt) line " ^ string_of_int t.line
              ^ ": expected a semicolon, got " ^ t.lexeme ^ " instead" )
      | _ ->
          Err.fail
            (parser, "(parse_expr_stmt) expected a semicolon, got EOF instead"))

(* block          → "{" declaration* "}" ; *)
and parse_block (parser : parser) : stmt result =
  let ( let* ) = Err.bind in
  let rec parse_block_inside (parser : parser) (stmts : stmt list) =
    if match_token parser Scanner.RIGHT_BRACE then
      Err.return (advance parser, stmts)
    else
      let* parser, stmt = parse_declaration parser in
      parse_block_inside parser (stmt :: stmts)
  in
  let* parser, _ =
    consume parser Scanner.LEFT_BRACE "(parse_block): expected an opening '{'"
  in
  let* parser, stmts = parse_block_inside parser [] in
  let stmts = List.rev stmts in
  Err.return (parser, Block stmts)

(* whileStmt      → "while" "(" expression ")" statement ; *)
and parse_while (parser : parser) : stmt result =
  let ( let* ) = Err.bind in
  let* parser, _ =
    consume parser Scanner.WHILE "(parse_while): expected 'while' token"
  in
  let* parser, _ =
    consume parser Scanner.LEFT_PAREN
      "(parse_while): expected '(' after 'while'"
  in
  let* parser, expr = parse_expr parser in
  let* parser, _ =
    consume parser Scanner.RIGHT_PAREN
      "(parse_while): expected ')' after condition expression"
  in
  let* parser, stmt = parse_stmt parser in
  Err.return (parser, While (expr, stmt))

(* forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
                    expression? ";"
                    expression? ")" statement ; *)
(* this is just syntactic sugar for a block with a while loop inside *)
and parse_for (parser : parser) : stmt result =
  let ( let* ) = Err.bind in
  let* parser, _ =
    consume parser Scanner.FOR "(parse_for): expected 'for' token"
  in
  let* parser, _ =
    consume parser Scanner.LEFT_PAREN "(parse_for): expected '(' after 'for'"
  in
  let* parser, init_stmt =
    match get_token parser with
    | Some t when t.kind = Scanner.LET ->
        let* parser, stmt = parse_vardeclaration parser in
        Err.return (parser, Some stmt)
    | Some t when t.kind = Scanner.SEMICOLON -> Err.return (advance parser, None)
    | _ ->
        let* parser, stmt = parse_expr_stmt parser in
        Err.return (parser, Some stmt)
  in
  let* parser, condition =
    match get_token parser with
    | Some t when t.kind = Scanner.SEMICOLON ->
        Err.return (parser, Literal (Value.VBool true))
    | _ -> parse_expr parser
  in
  let* parser, _ =
    consume parser Scanner.SEMICOLON
      "(parse_for): expected ';' after condition expression"
  in
  let* parser, increment =
    match get_token parser with
    | Some t when t.kind = Scanner.SEMICOLON -> Err.return (parser, None)
    | _ ->
        let* parser, expr = parse_expr parser in
        Err.return (parser, Some expr)
  in
  let* parser, _ =
    consume parser Scanner.RIGHT_PAREN "(parse_for): expected ')'"
  in
  let* parser, inside_stmt = parse_stmt parser in
  Err.return
    ( parser,
      Block
        (let list = match init_stmt with Some stmt -> [ stmt ] | None -> [] in
         let body =
           match increment with
           | Some expr -> Block ([ inside_stmt ] @ [ Expr expr ])
           | None -> inside_stmt
         in
         let list = While (condition, body) :: list in
         List.rev list) )

(* returnStmt     → "return" expression? ";" ; *)
and parse_return (parser : parser) : stmt result =
  let ( let* ) = Err.bind in
  let* parser, _ =
    consume parser Scanner.RETURN "(parse_return): expected 'return'"
  in
  let* parser, expr =
    if match_token parser Scanner.SEMICOLON then
      Err.return (parser, Literal Value.VNil)
    else parse_expr parser
  in
  let* parser, _ =
    consume parser Scanner.SEMICOLON "(parse_return): expected ';' after return"
  in
  Err.return (parser, Return expr)

(* stmt -> exprStmt | printStmt | block | ifStmt | whileStmt
           | forStmt | returnStmt *)
and parse_stmt (parser : parser) : stmt result =
  match get_token parser with
  | Some t when t.kind = Scanner.PRINT -> parse_print parser
  | Some t when t.kind = Scanner.LEFT_BRACE -> parse_block parser
  | Some t when t.kind = Scanner.IF -> parse_ifstmt parser
  | Some t when t.kind = Scanner.WHILE -> parse_while parser
  | Some t when t.kind = Scanner.FOR -> parse_for parser
  | Some t when t.kind = Scanner.RETURN -> parse_return parser
  | Some _ -> parse_expr_stmt parser
  | None -> Err.fail (parser, "(parse_stmt) unexpected EOF")

(* ifStmt         → "if" "(" expression ")" statement
                    ( "else" statement )? ; *)
and parse_ifstmt (parser : parser) : stmt result =
  let ( let* ) = Err.bind in
  let* parser, _ = consume parser Scanner.IF "(parse_ifstmt): expected 'if'" in
  let* parser, __LINE__ =
    consume parser Scanner.LEFT_PAREN "(parse_ifstmt): expected '(' after 'if'"
  in
  let* parser, expr = parse_expr parser in
  let* parser, _ =
    consume parser Scanner.RIGHT_PAREN
      "(parse_ifstmt): expected ')' after expression"
  in
  let* parser, stmt_if_true = parse_stmt parser in
  if match_token parser Scanner.ELSE then
    let* parser, stmt_if_false = parse_stmt (advance parser) in
    Err.return (parser, If (expr, stmt_if_true, Some stmt_if_false))
  else Err.return (parser, If (expr, stmt_if_true, None))

(* varDecl        → "let" IDENTIFIER ( "=" expression )? ";" ; *)
and parse_vardeclaration (parser : parser) : stmt result =
  let ( let* ) = Err.bind in
  let* parser, _ =
    consume parser Scanner.LET "(parse_vardeclaration) expected let"
  in
  let* parser, varname =
    consume parser Scanner.IDENTIFIER "expected a variable or function name"
  in
  match get_token parser with
  | Some t when t.kind = Scanner.EQUAL ->
      let parser = advance parser in
      let* parser, expr = parse_expr parser in
      let* parser, _ =
        consume parser Scanner.SEMICOLON
          "expected ';' after variable declaration"
      in
      Err.return (parser, Let (varname, Some expr))
  | _ ->
      let* parser, _ =
        consume parser Scanner.SEMICOLON
          "expected ';' after variable declaration"
      in
      Err.return (parser, Let (varname, None))

(* declaration -> varDecl | stmt *)
and parse_declaration (parser : parser) : stmt result =
  match get_token parser with
  | Some t when t.kind = Scanner.LET -> parse_vardeclaration parser
  | Some _ -> parse_stmt parser
  | None -> Err.fail (parser, "parse_declaration: unexpected EOF")

(* program -> declaration program | EOF *)
and parse_program (tokens : Scanner.token list) =
  let rec parse_program (parser : parser) stmts (errors : string list) =
    if is_end parser then (parser, stmts, errors)
    else
      match parse_declaration parser with
      | Err.Ok (parser, stmt) ->
          parse_program parser (stmt :: stmts) errors
          (* założenie jest takie, że parse_stmt zwraca parser który wskazuje na następny token po tym co ostatnio sparsował *)
      | Err.Error (parser, message) ->
          let parser = synchronize parser in
          parse_program parser stmts (message :: errors)
  in
  let _, stmts, errors = parse_program (mk_parser tokens) [] [] in
  if List.length errors = 0 then Err.Ok (List.rev stmts)
  else Err.Error (List.rev errors)

let parse = parse_program
