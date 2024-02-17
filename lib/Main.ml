let _get_stmts line =
  let scanner_result = Scanner.scan_tokens line in
  match scanner_result with
  | Err.Error message ->
      let _ = Err.static_errors [ message ] in
      failwith "scanner error"
  | Err.Ok scanned -> (
      let parser_result = scanned |> Parser.parse in
      match parser_result with
      | Err.Error messages ->
          let _ = Err.static_errors messages in
          failwith "parser error"
      | Err.Ok stmts -> stmts)
[@@ocaml.warning "-32"]

let run_once line =
  let scanner_result = Scanner.scan_tokens line in
  match scanner_result with
  | Err.Error message ->
      let _ = Err.static_errors [ message ] in
      ""
  | Err.Ok scanned -> (
      let parser_result = scanned |> Parser.parse in
      match parser_result with
      | Err.Error messages ->
          let _ = Err.static_errors messages in
          ""
      | Err.Ok stmts -> (
          let interpreter_result = Interpreter.interpret stmts in
          match interpreter_result with
          | Err.Ok res -> Interpreter.string_of_value res
          | Err.Error message ->
              Err.runtime_error message;
              ""))

let run line =
  let rec run line env =
    let scanner_result = Scanner.scan_tokens line in
    match scanner_result with
    | Err.Error message ->
        let _ = Err.static_errors [ message ] in
        ""
    | Err.Ok scanned -> (
        let parser_result = scanned |> Parser.parse in
        match parser_result with
        | Err.Error messages ->
            let _ = Err.static_errors messages in
            print_string ">>> ";
            let line = read_line () in
            run line env
        | Err.Ok stmts -> (
            let interpreter_result = Interpreter.interpret_stmts env stmts in
            match interpreter_result with
            | Err.Ok (env, res) ->
                print_endline (Interpreter.string_of_value res);
                print_string ">>> ";
                let line = read_line () in
                run line env
            | Err.Error message ->
                Err.runtime_error message;
                print_string ">>> ";
                let line = read_line () in
                run line env))
  in
  run line Interpreter.fresh_env

let read_file_into_string (filename : string) : string option =
  try
    let open Stdlib in
    let ic = open_in filename in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Some (Bytes.to_string s)
  with Sys_error msg ->
    Printf.eprintf "Error reading file: %s\n" msg;
    None

let run_file (filename : string) =
  match read_file_into_string filename with
  | Some s -> run_once s
  | None -> failwith "Error reading file"

let run_prompt () =
  print_string ">>> ";
  let line = read_line () in
  run line

let main () =
  let argc = Array.length Sys.argv in
  if argc > 2 then (
    print_endline "Usage: actinium [script]";
    exit 64)
  else if argc = 2 then
    let file_name = Sys.argv.(1) in
    run_file file_name
  else run_prompt ()
