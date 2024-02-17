type ('a, 'b) t = Ok of 'a | Error of 'b

val return : 'a -> ('a, 'b) t
val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

val fail : 'a -> ('b, 'a) t
(** Błąd *)

val catch : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
(** Przechwytywanie błędu *)

val run : ('a, 'b) t -> ('a, 'b) t

(* non-monad error functions *)
val static_error : int -> string -> unit
val static_errors : string list -> unit list
val runtime_error : string -> unit