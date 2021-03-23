(** Pretty-printing. *)

open Lang

val pat : pat -> string
(** Pretty-prints a pattern. *)

val typ : typ -> string
(** Pretty-prints a type. *)

val exp : exp -> string
(** Pretty-prints an expression. *)

val res : res -> string
(** Pretty-prints a result. *)

val value : value -> string
(** Pretty-prints a value. *)

val example : example -> string
(** Pretty-prints an example. *)
