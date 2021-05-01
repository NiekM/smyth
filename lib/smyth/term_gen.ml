open Lang
open Nondet.Syntax

(*******************************************************************************
 * Identifier generation
 *)

let fresh_ident idents first_char =
  let extract_number (ident : string) : int option =
    let ident_len =
      String.length ident
    in
      if ident_len > 0 && Char.equal (String.get ident 0) first_char then
        ident
          |> StringLabels.sub ~pos:1 ~len:(ident_len - 1)
          |> int_of_string_opt
      else
        None
  in
  let fresh_number : int =
    idents
      |> List.filter_map extract_number
      |> List2.maximum
      |> Option2.map ((+) 1)
      |> Option2.with_default 1
  in
    String.make 1 first_char ^ string_of_int fresh_number

let rec fresh_idents n idents first_char =
  match n with
  | 0 -> []
  | n ->
    let ident =
      fresh_ident idents first_char
    in
      ident :: fresh_idents (n - 1) (ident :: idents) first_char

let function_char =
  'f'

let variable_char =
  'x'

let match_char =
  'y'

(*******************************************************************************
 * Polymorphic helpers
 *)

let simple_types : datatype_ctx -> type_ctx -> typ Nondet.t =
  fun sigma gamma ->
    let polymorphic_variables =
      gamma
        |> Type_ctx.all_poly
        |> List.map (fun x -> TVar x)
    in
    (* Uncomment for monomorphic instantiations *)
    (*
    let monomorphic_datatypes =
      sigma
        |> List2.concat_map
             ( fun (_name, (type_params, _)) ->
                 if type_params = [] then
                   [TData (name, [])]
                 else
                   []
             )
    in
    *)
    let rank_zero_nd =
      Nondet.from_list
        ( polymorphic_variables
            (* @ monomorphic_datatypes *)
        )
    in
    let datatypes_nd =
      let* (datatype_name, datatype_params) =
        sigma
          |> List.map
               ( fun (name, (type_params, _)) ->
                   ( name
                   ,   type_params
                   )
               )
          |> Nondet.from_list
      in
      datatype_params
        |> List.map (fun _ -> rank_zero_nd)
        |> Nondet.one_of_each
        |> Nondet.map
             ( fun args ->
                 TData (datatype_name, args)
             )
    in
    Nondet.union
      [ Nondet.from_list polymorphic_variables
      ; datatypes_nd
      ]

(** [goal_match goal_type binding] finds all forall- and argument-peeled instantiations
    of [binding] matching [goal_type] *)
let goal_match : typ -> type_binding -> (exp * string list * typ list * typ) Nondet.t =
  fun goal_type (name, (tau, _)) ->
    (* Peel forall *)
    let params, bound_type =
      Type.peel_forall tau
    in
    (* Generate fresh identifiers *)
    let fresh_vars =
      fresh_idents (List.length params) (Type.free_vars tau) 'a'
    in
    (* Instantiate type with fresh identifiers *)
    let fresh_tau =
      Type.subst (List.combine params @@ List.map (fun x -> TVar x) fresh_vars) bound_type
    in
    let exp =
      Desugar.app
        (EVar name)
        (List.map (fun x -> EAType (TVar x)) fresh_vars)
    in
    let* exp, args, ret =
      Type.applications exp fresh_tau |> Nondet.from_list
    in
      (* TODO: should fresh_vars contain "*", or should it only be added for unification? *)
      match Type.unify ("*" :: fresh_vars) ret goal_type with
      | Error _ -> Nondet.none
      | Ok th ->
        Nondet.pure
          ( Exp.subst th exp
          , List.filter (fun x -> not (List.mem_assoc x th)) fresh_vars
          , List.map (Type.subst th) args
          , Type.subst th ret
          )

let instantiations : typ Nondet.t -> typ -> type_binding -> (exp * typ list) Nondet.t =
  fun types goal_type binding ->
    let* exp, params, args, _ =
      goal_match goal_type binding
    in
      params
        |> List.map (fun _ -> types)
        |> Nondet.one_of_each
        |> Nondet.map
          ( fun type_args ->
            let th =
              List.combine params type_args
            in
              ( Exp.subst th exp
              , List.map (Type.subst th) args
              )
          )

(*******************************************************************************
 * Term permission helpers
 *)


let parts (k : int) : relevance list Nondet.t =
  let+ i =
    Nondet.from_list (List2.range ~low:1 ~high:k)
  in
    List2.repeat (i - 1) Not @ [Must] @ List2.repeat (k - i) May

(*******************************************************************************
 * Caching
 *)

(* Types *)

type gen_input =
  { sigma : datatype_ctx
  ; term_kind : term_kind
  ; term_size : int
  ; rel_binding : type_binding option
  ; goal : gen_goal
  }

(* TODO: maybe move hashing to its own file? *)
(* Hashing *)

let hash ({ term_kind; term_size; rel_binding; goal } : gen_input) : string =
  let rec hash_type (tau : typ) : string =
    match tau with
      | TArr (tau1, tau2) ->
          "[" ^ hash_type tau1 ^ ">" ^ hash_type tau2 ^ "]"

      | TTuple taus ->
          taus
            |> List.map hash_type
            |> String.concat ","
            |> (fun s -> "(" ^ s ^ ")")

      | TData (d, type_args) ->
          d ^ "<" ^ String.concat "," (List.map hash_type type_args)

      | TForall (a, bound_type) ->
          "f:" ^ a ^ "." ^ hash_type bound_type

      | TVar x ->
          x
  in
  let hash_bind_spec (bind_spec : bind_spec) : string =
    match bind_spec with
      | NoSpec -> "."
      | Rec name -> "r:" ^ name
      | Arg name -> "a:" ^ name
      | Dec name -> "d:" ^ name
  in
  let hash_rel (rel : relevance) : string =
    match rel with
      | Must -> "m"
      | May -> "?"
      | Not -> "n"
  in
  let tk_string =
    match term_kind with
      | E -> "E"
      | I -> "I"
  in
  let ts_string =
    string_of_int term_size
  in
  let rb_string =
    match rel_binding with
      | None ->
          ""

      | Some (name, (tau, (bind_spec, rel))) ->
          name ^ ";" ^ hash_type tau ^ ";" ^ hash_bind_spec bind_spec ^ hash_rel rel
  in
  let goal_string =
    let (gamma, goal_type, goal_dec) =
      goal
    in
    (* Sigma never changes, so no need to keep track of it in the cache *)
    let gamma_type_string =
      gamma
        |> Type_ctx.all_type
        |> List.map
             ( fun (name, (tau, (bind_spec, rel))) ->
                 name ^ "`" ^ hash_type tau ^ "`" ^ hash_bind_spec bind_spec ^ "`" ^ hash_rel rel
             )
        |> String.concat "&"
    in
    let gamma_poly_string =
      gamma
        |> Type_ctx.all_poly
        |> List.map
             ( fun name ->
                 name
             )
        |> String.concat "&"
    in
    let gt_string =
      hash_type goal_type
    in
    let gd_string =
      Option2.with_default "." goal_dec
    in
      gamma_type_string ^ "!" ^ gamma_poly_string ^ "!"
        ^ gt_string ^ "!" ^ gd_string
  in
    tk_string ^ "!" ^ ts_string ^ "@" ^ rb_string ^ "#" ^ goal_string

(* Caching *)

let gen_cache : (string, exp Nondet.t) Hashtbl.t =
  Hashtbl.create 100

let lookup (gen_input : gen_input) : exp Nondet.t option =
  gen_input
    |> hash
    |> Hashtbl.find_opt gen_cache

let record (gen_input : gen_input) (solution : exp Nondet.t) : exp Nondet.t =
  Hashtbl.add gen_cache (hash gen_input) solution;
  solution

(*******************************************************************************
 * Term generation
 *)

let rec get_head : exp -> exp =
  function
  | EApp (_, exp, EAExp _)
  | EProj (_, _, exp) -> get_head exp
  | exp -> exp

(* --- Important info about the term generation helpers! ---
 *
 * Do NOT call gen_e, rel_gen_e, gen_i, or rel_gen_i from anywhere EXCEPT inside
 * the actual gen function. The gen function handles caching; no other code
 * should worry about directly manipulating the cache.
 *
 * So, if, for example, genE wants to recursively call itself, it should
 * actually do so indirectly via calling gen with the appropriate arguments.
 *
 * Also, these helpers assume term_size > 0.
 *)

let rec gen_e
  (sigma : datatype_ctx)
  (term_size : int)
  ((gamma, goal_type, goal_dec) : gen_goal)
  : exp Nondet.t =
    match Type_ctx.peel_type gamma with
      | Some (binding, gamma_rest) ->
          Nondet.union
            [ gen
                { sigma
                ; term_kind = E
                ; term_size
                ; rel_binding = Some binding
                ; goal = (gamma_rest, goal_type, goal_dec)
                }
            ; gen
                { sigma
                ; term_kind = E
                ; term_size
                ; rel_binding = None
                ; goal = (gamma_rest, goal_type, goal_dec)
                }
            ]

      | None ->
          Nondet.none

(* TODO: Make sure that second arguments can also be structurally recursive,
  this would require more expressive bindspecs,
  that specify that an argument is argument to multiple functions, each in a different position,
  so maybe a list of strings where the index in the list determines the argument number *)
(* TODO: maybe even structural recursion on part of an argument, for an input-stack of projections and arguments? *)


and rel_gen_e
  (sigma : datatype_ctx)
  (term_size : int)
  (rel_binding : type_binding)
  ((gamma, goal_type, _goal_dec) : gen_goal)
  : exp Nondet.t =
    let combined_gamma =
      Type_ctx.add_type rel_binding gamma
    in
    let app_combine
      (exp : exp) (args : exp list) : exp Nondet.t =
        (* TODO: remove get_head *)
        let head =
          get_head exp
        in
        let bindspec =
          Type.bind_spec combined_gamma head
        in
        let special =
          match bindspec with | Rec _ -> true | _ -> false
        in
        let* _ =
          (* Recursive calls should have arguments and the first one should be structurally decreasing *)
          Nondet.guard @@ if List.length args = 0 then not special else
            Type.structurally_decreasing combined_gamma ~head ~arg:(List.hd args)
        in
          Nondet.pure @@ Exp.fill_holes (List.fold_left (fun m (k, x) -> Hole_map.add k x m) Hole_map.empty (List.mapi Pair2.pair args)) exp
    in
    let rel_head_nd =
      let* (exp, taus) =
        instantiations (simple_types sigma combined_gamma) goal_type rel_binding
      in
      let arg_size =
        List.length taus
      in
      let* partition =
        Nondet.from_list @@
          Int2.partition_permutations
            ~n:(term_size - arg_size - 1) (* -1 for each application and -1 for the head *)
            ~k:arg_size
      in
        (* We apply the relevant binding to newly generated arguments.
          They don't need any special treatment, because we already used the relevant binding. *)
        Nondet.join @@ Nondet.map (app_combine exp) @@
          Nondet.one_of_each @@
            List.map2
              begin fun tau n ->
                gen
                  { sigma
                  ; term_kind = I
                  ; term_size = n
                  ; rel_binding = None
                  ; goal =
                    ( combined_gamma
                    , tau
                    , None
                    )
                  }
              end
              taus
              partition
    in
    let rel_arg_nd =
      let* (exp, taus) =
        combined_gamma (* NOTE: should this just be gamma? *)
          |> Type_ctx.all_type
          |> List.map
            @@ instantiations (simple_types sigma combined_gamma) goal_type
          |> Nondet.union
      in
      let arg_size =
        List.length taus
      in
      let* partition =
        Nondet.from_list @@
          Int2.partition_permutations
            ~n:(term_size - arg_size - 1) (* -1 for each application and -1 for the head *)
            ~k:arg_size
      in
      let* part =
        parts arg_size
      in
        Nondet.join @@ Nondet.map (app_combine exp) @@
          Nondet.one_of_each @@
            List2.map3
              begin fun tau n tp ->
                genp_i sigma n tp rel_binding (gamma, tau, None)
              end
              taus
              partition
              part
    in
      Nondet.union
        [ rel_head_nd
        ; rel_arg_nd
        ]

and genp_i
  (sigma : datatype_ctx)
  (term_size : int)
  (rel : relevance)
  (rel_binding : type_binding)
  ((gamma, goal_type, goal_dec) : gen_goal)
  : exp Nondet.t =
    let (rel_binding', gamma') =
      match rel with
        | Must ->
            (Some rel_binding, gamma)

        | May ->
            (None, Type_ctx.add_type rel_binding gamma)

        | Not ->
            (None, gamma)
    in
      gen
        { sigma
        ; term_kind = I
        ; term_size
        ; rel_binding = rel_binding'
        ; goal = (gamma', goal_type, goal_dec)
        }

and gen_i
  (sigma : datatype_ctx)
  (term_size : int)
  ((gamma, goal_type, goal_dec) as goal : gen_goal)
  : exp Nondet.t =
    let* _ =
      Nondet.guard (Option.is_none goal_dec)
    in
      match Type_ctx.peel_type gamma with
        | Some (binding, gamma_rest) ->
            Nondet.union
              [ gen
                  { sigma
                  ; term_kind = I
                  ; term_size
                  ; rel_binding = Some binding
                  ; goal = (gamma_rest, goal_type, goal_dec)
                  }
              ; gen
                  { sigma
                  ; term_kind = I
                  ; term_size
                  ; rel_binding = None
                  ; goal = (gamma_rest, goal_type, goal_dec)
                  }
              ]

        | None -> rel_gen_i sigma term_size None goal 

and rel_gen_i
  (sigma : datatype_ctx)
  (term_size : int)
  (rel_binding : type_binding option)
  ((gamma, goal_type, goal_dec) as goal : gen_goal)
  : exp Nondet.t = 
    let* _ =
      Nondet.guard (Option.is_none goal_dec)
    in
    (* All E-forms are I-forms *)
    let e_option =
      gen
        { sigma
        ; term_kind = E
        ; term_size
        ; rel_binding = rel_binding
        ; goal
        }
    in
    let i_option =
      match goal_type with
        | TArr (tau1, tau2) ->
            let idents =
              Type_ctx.names gamma
            in
            let f_name =
              fresh_ident idents function_char
            in
            let arg_name =
              fresh_ident idents variable_char
            in
            let+ body =
              gen
                { sigma
                ; term_kind = I
                ; term_size = term_size - 1 (* -1 for lambda *)
                ; rel_binding = rel_binding
                ; goal =
                    ( Type_ctx.concat_type
                        [ (arg_name, (tau1, (Arg f_name, May)))
                        ; (f_name, (goal_type, (Rec f_name, May)))
                        ]
                        gamma
                    , tau2
                    , None
                    )
                }
            in
              EFix (Some f_name, PatParam (PVar arg_name), body)

        | TTuple taus ->
            let tuple_size =
              List.length taus
            in
            let* partition =
              Nondet.from_list @@
                Int2.partition_permutations
                  ~n:(term_size - 1) (* -1 for tuple *)
                  ~k:tuple_size
            in
            (
              match rel_binding with
                | None -> 
                  Nondet.map (fun es -> ETuple es) @@
                    Nondet.one_of_each @@
                      List.map2
                        begin fun tau n ->
                          gen
                            { sigma
                            ; term_kind = I
                            ; term_size = n
                            ; rel_binding = None
                            ; goal = (gamma, tau, None)
                            }
                        end
                        taus
                        partition
                | Some rb ->
                  let* part =
                    parts tuple_size
                  in
                    Nondet.map (fun es -> ETuple es) @@
                      Nondet.one_of_each @@
                        List2.map3
                          begin fun tau n tp ->
                            genp_i sigma n tp rb (gamma, tau, None)
                          end
                          taus
                          partition
                          part
            )
                        
        | TData (datatype_name, datatype_args) ->
            let* (ctor_name, arg_type) =
              List.assoc_opt datatype_name sigma
                |> Option2.map (snd >> Nondet.from_list)
                |> Option2.with_default Nondet.none
            in
            let+ arg =
              gen
                { sigma
                ; term_kind = I
                ; term_size = term_size - 1 (* -1 for constructor *)
                ; rel_binding = rel_binding
                ; goal = (gamma, arg_type, None)
                }
            in
              ECtor (ctor_name, datatype_args, arg)

        | TForall (a, bound_type) ->
            let+ body =
              gen
                { sigma
                ; term_kind = I
                ; term_size = term_size - 1 (* -1 for lambda *)
                ; rel_binding = rel_binding
                ; goal =
                    ( Type_ctx.add_poly
                        a
                        gamma
                    , bound_type
                    , None
                    )
                }
            in
              EFix (None, TypeParam a, body)

        | TVar _ ->
            (* No introduction form for a type variable *)
            Nondet.none
    in
      Nondet.union [e_option; i_option]

(* TODO: skip term_generation for goal types isomorphic to Unit *)
and gen (gen_input : gen_input) : exp Nondet.t =
  if gen_input.term_size <= 0 then
    Nondet.none
  else
    match lookup gen_input with
      | Some solution ->
          solution

      | None ->
          let
            { term_size; sigma; goal; _ } =
              gen_input
          in
            record gen_input @@
              Nondet.dedup @@
                begin match (gen_input.term_kind, gen_input.rel_binding) with
                  | (E, None) ->
                      gen_e sigma term_size goal

                  | (E, Some rb) ->
                      rel_gen_e sigma term_size rb goal

                  | (I, None) ->
                      gen_i sigma term_size goal

                  | (I, Some rb) ->
                      rel_gen_i sigma term_size (Some rb) goal
                end

(*******************************************************************************
 * Term generation exports
 *)

let clear_cache _ =
  Hashtbl.reset gen_cache

let up_to term_kind sigma max_size goal =
  List2.range ~low:1 ~high:max_size
    |> List.map
         begin fun term_size ->
           gen
             { sigma
             ; term_kind = term_kind
             ; rel_binding = None
             ; term_size
             ; goal
             }
         end
    |> Nondet.union
