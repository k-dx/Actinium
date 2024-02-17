type token_kind =
  (* Single-character tokens. *)
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | LEFT_BRACKET
  | RIGHT_BRACKET
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  (* One or two character tokens. *)
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  (* Literals. *)
  | IDENTIFIER
  | STRING
  | NUMBER
  (* Keywords. *)
  | AND
  | ELSE
  | FALSE
  | FN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | TRUE
  | LET
  | WHILE
  (**)
  | EOF

type token = {
  kind : token_kind;
  lexeme : string;
  literal : Value.t option;
  line : int;
}

type scanner = {
  source : string;
  tokens : token list;
  start : int;
  current : int;
  line : int;
}

type 'a result = ('a, string) Err.t

let keywords =
  [
    ("and", AND);
    ("else", ELSE);
    ("false", FALSE);
    ("fn", FN);
    ("for", FOR);
    ("if", IF);
    ("nil", NIL);
    ("or", OR);
    ("print", PRINT);
    ("return", RETURN);
    ("true", TRUE);
    ("let", LET);
    ("while", WHILE);
  ]

let advance (scanner : scanner) : scanner =
  {
    source = scanner.source;
    tokens = scanner.tokens;
    start = scanner.start;
    current = scanner.current + 1;
    line = scanner.line;
  }

let get_char (scanner : scanner) : char option =
  if scanner.current >= String.length scanner.source then None
  else Some (String.get scanner.source scanner.current)

let peek (scanner : scanner) : char option =
  if scanner.current + 1 >= String.length scanner.source then None
  else Some (String.get scanner.source (scanner.current + 1))

let match_next (expected : char) (scanner : scanner) : bool =
  match peek scanner with None -> false | Some c -> c = expected

let advance_line (scanner : scanner) : scanner =
  {
    source = scanner.source;
    tokens = scanner.tokens;
    start = scanner.start;
    current = scanner.current;
    line = scanner.line + 1;
  }

let get_lexeme (scanner : scanner) : string =
  String.sub scanner.source scanner.start (scanner.current - scanner.start)

(* we want to ignore characters until newline \n or EOF *)
let rec consume_comment (scanner : scanner) : scanner =
  match scanner |> get_char with
  | None -> scanner
  | Some c ->
      if c = '\n' then scanner else scanner |> advance |> consume_comment

let add_token ?(literal = None) (scanner : scanner) (token_kind : token_kind) :
    scanner =
  let token =
    {
      kind = token_kind;
      lexeme = get_lexeme scanner;
      literal;
      line = scanner.line;
    }
  in
  {
    source = scanner.source;
    tokens = token :: scanner.tokens;
    start = scanner.start;
    current = scanner.current;
    line = scanner.line;
  }

let add_double_token (scanner : scanner) (token_kind_if_double : token_kind)
    (token_kind_if_single : token_kind) : scanner =
  match scanner |> get_char with
  | Some c when c = '=' -> add_token (advance scanner) token_kind_if_double
  | _ -> add_token scanner token_kind_if_single

(* we assume that previous char in scanner is '"' *)
let rec add_string ?(str = "") (scanner : scanner) : scanner result =
  match scanner |> get_char with
  | None -> Err.fail "string is not terminated (at EOF)"
  | Some c ->
      if c = '"' then
        Err.return
          (add_token scanner STRING ~literal:(Some (Value.VString str)))
      else add_string (advance scanner) ~str:(str ^ String.make 1 c)

let is_digit = function '0' .. '9' -> true | _ -> false
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let rec add_number ?(number = "") (scanner : scanner) : scanner =
  match get_char scanner with
  | Some c when is_digit c ->
      add_number ~number:(number ^ String.make 1 c) (advance scanner)
  | _ ->
      add_token
        ~literal:(Some (Value.VInt (int_of_string number)))
        scanner NUMBER

let rec add_identifier ?(identifier = "") (scanner : scanner) : scanner =
  match get_char scanner with
  | Some c when is_alpha c ->
      add_identifier
        ~identifier:(identifier ^ String.make 1 c)
        (advance scanner)
  | _ ->
      let text = get_lexeme scanner in
      let token_type =
        match List.assoc_opt text keywords with
        | None -> IDENTIFIER
        | Some t -> t
      in
      add_token scanner token_type

let scan_token (scanner : scanner) : scanner result =
  let ( let* ) = Err.bind in
  match get_char scanner with
  | None -> Err.return (add_token scanner EOF |> advance)
  | Some c -> (
      match c with
      | '(' -> Err.return (add_token (advance scanner) LEFT_PAREN)
      | ')' -> Err.return (add_token (advance scanner) RIGHT_PAREN)
      | '{' -> Err.return (add_token (advance scanner) LEFT_BRACE)
      | '}' -> Err.return (add_token (advance scanner) RIGHT_BRACE)
      | '[' -> Err.return (add_token (advance scanner) LEFT_BRACKET)
      | ']' -> Err.return (add_token (advance scanner) RIGHT_BRACKET)
      | ',' -> Err.return (add_token (advance scanner) COMMA)
      | '.' -> Err.return (add_token (advance scanner) DOT)
      | '-' -> Err.return (add_token (advance scanner) MINUS)
      | '+' -> Err.return (add_token (advance scanner) PLUS)
      | ';' -> Err.return (add_token (advance scanner) SEMICOLON)
      | '*' -> Err.return (add_token (advance scanner) STAR)
      | '/' ->
          Err.return
            (if match_next '/' scanner then
               consume_comment (scanner |> advance |> advance)
             else add_token (advance scanner) SLASH)
      | '\n' -> Err.return (scanner |> advance_line |> advance)
      | '"' ->
          let* scanner = scanner |> advance |> add_string in
          Err.return (scanner |> advance)
      | '!' -> Err.return (add_double_token (advance scanner) BANG_EQUAL BANG)
      | '<' -> Err.return (add_double_token (advance scanner) LESS_EQUAL LESS)
      | '>' ->
          Err.return (add_double_token (advance scanner) GREATER_EQUAL GREATER)
      | '=' -> Err.return (add_double_token (advance scanner) EQUAL_EQUAL EQUAL)
      | ' ' | '\r' | '\t' -> Err.return (scanner |> advance)
      | c when is_digit c -> Err.return (add_number scanner)
      | c when is_alpha c -> Err.return (add_identifier scanner)
      | _ -> Err.fail ("invalid character at line " ^ string_of_int scanner.line)
      )

let mk_scanner (source : string) : scanner =
  { source; tokens = []; start = 0; current = 0; line = 0 }

let is_end (scanner : scanner) : bool =
  scanner.current >= String.length scanner.source

let scan_tokens (source : string) : token list result =
  let ( let* ) = Err.bind in
  let rec scan_tokens (scanner : scanner) : scanner result =
    if is_end scanner then Err.return scanner
    else
      let* token = { scanner with start = scanner.current } |> scan_token in
      scan_tokens token
  in
  let* res = scan_tokens (mk_scanner source) in
  Err.return (List.rev res.tokens)
