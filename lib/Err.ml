type ('a, 'b) t = Ok of 'a | Error of 'b

let return x = Ok x
let bind m f = match m with Error message -> Error message | Ok x -> f x
let fail message = Error message
let catch m f = match m with Ok x -> Ok x | Error message -> f message
let run m = m

(* non-monad error functions *)
let static_error line message =
  Printf.printf "[line %d] Error: %s\n" line message

let static_errors messages =
  List.map (fun message -> Printf.printf "Error: %s\n" message) messages

let runtime_error message = Printf.printf "Runtime error: %s\n" message