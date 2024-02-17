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

type 'a result = ('a, string) Err.t

val scan_tokens : string -> token list result