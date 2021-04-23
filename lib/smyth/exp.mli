(** Expression helpers. *)

open Lang

val syntactically_equal : exp -> exp -> bool
(** [syntactically_equal e1 e2] determines if [e1] and [e2] have the same
    abstract syntax tree. *)

val largest_hole : exp -> hole_name
(** [largest_hole e] returns the greatest-numbered hole in [e] *)

val has_special_recursion : exp -> bool
(** [has_special_recursion e] determines if [e] or one of its subexpressions has
    an application marked as "special". See {!Lang.exp} for details about
    "special" applications. *)

val fill_hole : (hole_name * exp) -> exp -> exp
(** [fill_hole  (h, e) root] replaces a hole ??{_h} with the expression [e]
    in the expression [root]. *)

val fill_holes : hole_filling -> exp -> exp
(** [fill_hole hf root] replaces holes with the expressions according to hole
    filling [hf] in the expression [root]. *)

val sub_sketches : exp -> exp Nondet.t
(** [sub_sketches exp] returns all possible sketches that can be unified with
    [exp], including [exp] itself. *)

val sub_expressions : exp -> exp Nondet.t
(** [sub_expressions exp] returns all subexpressions that can be found in
    [exp], including [exp] itself. *)

val subst : Type.subst -> exp -> exp
(** [subst th exp] performs many type substitutions at once, replacing,
    for every pair [before, after] in [th], all free occurrences of
    [before] in [exp] with [after]. *)