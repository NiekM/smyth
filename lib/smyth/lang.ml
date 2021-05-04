(** The syntax of {e Core Smyth}, as defined in {b Figure 3} of the ICFP 2020
    paper. *)

(** The type of hole "names," which is used to identify holes for synthesis. *)
type hole_name =
  int

(** A map with domain {!hole_name}. *)
module Hole_map =
  Map.Make
    ( struct
        type t = hole_name
        let compare = Int.compare end
    )

(** An abbreviation for using {!Hole_map}s. *)
type 'a hole_map =
  'a Hole_map.t

(** Expression types. *)
type typ =
  | TArr of typ * typ (** Arrow type *)
  | TTuple of typ list (** Tuple type *)
  | TData of string * typ list (** Datatype *)
  | TForall of string * typ (** Universal quantification *)
  | TVar of string (** Type variable *)

(** Patterns. *)
type pat =
  | PVar of string (** Variable pattern *)
  | PTuple of pat list (** Tuple pattern *)
  | PWildcard (** Wildcard pattern *)

(** The types of valid parameters in a function signature. *)
type param =
  | PatParam of pat
  | TypeParam of string

(** The types of valid arguments in a function application. *)
type exp_arg =
  | EAExp of exp
  | EAType of typ

(** Expressions.

    - If the "name" field of a fix expression is [None], then the fix is a
      non-recursive lambda.
    *)
and exp =
  | EFix of string option * param * exp
  (** Fix expressions [(name, param, body)]*)
  | EApp of exp * exp_arg
  (** Applications [(head, arg)] *)
  | EVar of string
  (** Variables *)
  | ETuple of exp list
  (** Tuples *)
  | EProj of int * int * exp
  (** Tuple projections [(n, i, arg)] *)
  | ECtor of string * typ list * exp
  (** Constructors [(name, type args, arg)] *)
  | ECase of exp * (string * (pat * exp)) list
  (** Case expressions [(scrutinee, branches)] *)
  | EHole of hole_name
  (** Hole expressions *)
  | EAssert of exp * exp
  (** Assertions *)
  | ETypeAnnotation of exp * typ
  (** Type annotations *)

(** The types of valid arguments in a result function application. *)
type res_arg =
  | RARes of res
  | RAType of typ

(** Results.

    - Determinate results: [RFix, RTuple, RCtor].
    - Indeterminate results: [RHole, RApp, RProj, RCase, RCtorInverse]. *)
and res =
  | RFix of env * (string option) * param * exp
  (** Fix closures *)
  | RTuple of res list
  (** Tuples *)
  | RCtor of string * res
  (** Constructors *)
  | RHole of env * hole_name
  (** Hole closures *)
  | RApp of res * res_arg
  (** Applications *)
  | RProj of int * int * res
  (** Tuple projections *)
  | RCase of env * res * (string * (pat * exp)) list
  (** Case results *)
  | RCtorInverse of string * res
  (** Inverse constructors *)

(** Environments: [(result bindings, type variable bindings)]. *)
and env =
  (string * res) list * (string * typ) list

(** Binding specifications. *)
type bind_spec =
  | NoSpec (* No bindspec *)
  | Rec of string (* "Recursive" bindspec *)
  | Arg of string (* "Argument of" bindspec *)
  | Dec of string (* "Decreasing on" bindspec *)

(** Relevance of bindings. *)
type relevance =
  | Must (* Must be used as a relevant binding *)
  | May (* May be used as a relevant binding *)
  (* TODO: do we need Not or can we just remove those bindings? Or pre-pass to remove them? *)
  | Not (* Must not be used *)

(* Term kinds. *)
type term_kind =
  | E (* Elimination form *)
  | I (* Introduction form *)

(** Type bindings for type contexts. *)
type type_binding =
  string * (typ * (bind_spec * relevance))

(** Polymorphic name "bindings" for type contexts (just the name of the variable
    is needed in the type context). *)
type poly_binding =
  string

(** Type contexts. *)
type type_ctx =
  type_binding list * poly_binding list

(** Datatype contexts. *)
type datatype_ctx =
  (string * (string list * (string * typ) list)) list

(** Hole contexts: [(hole name, type context, typ, decrease requirement, match depth)].
    The "decrease requirement", if present, is a function that expressions must
    be decreasing on to fill the hole in question. *)
type hole_ctx =
  (hole_name * (type_ctx * typ * string option * int)) list

(** "Simple" values. *)
type value =
  | VTuple of value list (** Tuples *)
  | VCtor of string * value (** Constructors *)

(** Examples. *)
type example =
  | ExTuple of example list (** Tuples *)
  | ExCtor of string * example (** Constructors *)
  | ExInputOutput of value * example (** Input-output examples *)
  | ExTop (** Top (wildcard) examples *)

(** Example constraints, also known as "worlds." *)
type world =
  env * example

(** Multiple example constraints (worlds). *)
type worlds =
  world list

(** Hole fillings. *)
type hole_filling =
  exp hole_map

(** Unfilled holes, also known as "unsolved constraints." *)
type unsolved_constraints =
  worlds hole_map

(** Constraints. *)
type constraints =
  hole_filling * unsolved_constraints

(** Resumption assertions, as defined in {b Figure 7}. *)
type resumption_assertion =
  res * value

(** Multiple resumption assertions. *)
type resumption_assertions =
  resumption_assertion list

(** Term generation ("guessing") goals. *)
type gen_goal =
  { gamma : type_ctx
  ; goal_type : typ
  ; free_vars : string list
  ; goal_dec : string option
  }

(** Basic synthesis goals. *)
type synthesis_goal =
  gen_goal * worlds

(** Full notion of synthesis goals, including a hole name. *)
type fill_goal =
  hole_name * synthesis_goal

(** Parameters for synthesis. *)
type synthesis_params =
  { max_scrutinee_size : int
  ; max_match_depth : int
  ; max_term_size : int
  }
