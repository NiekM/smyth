type error =
  | ParseError of (Parse.context, Parse.problem) Bark.dead_end list
  | TypeError of (Lang.exp * Type.error)
  | EvalError of string
  | TimedOut of float
  | NoSolutions
  | PartialNotSubsetFull

type 'a response =
  ('a, error) result

(* Program Helpers *)

let parse_program : string -> Desugar.program response =
  Bark.run Parse.program
    >> Result.map_error (fun e -> ParseError e)

open Lang

let rec eta_expand (gamma : type_ctx) (tau : typ) : exp =
  match tau with
  | TArr (tau1, tau2) ->
    let f_name =
      Term_gen.fresh_ident
        gamma
        Term_gen.function_char
    in
    let x_name =
      Term_gen.fresh_ident
        gamma
        (* NOTE: it turns out it is more efficient to not use 'x' here *)
        'z'
    in
    let gamma' =
      Type_ctx.concat_type
        [ (f_name, (TArr (tau1, tau2), (Rec f_name, May)))
        ; (x_name, (tau1, (Arg f_name, May)))
        ]
        gamma
    in
    EFix (Some f_name, PatParam (PVar x_name), eta_expand gamma' tau2)
  | _ -> EHole (Fresh.gen_hole ())

let eta_expand_all delta =
  delta
    (* NOTE: apparently, the order of the hole_ctx matters
      , reversing it ensures the same results as before *)
    |> List.rev
    |> List.filter_map (fun (i, (gamma, tau, _, _)) ->
      match tau with
      | TArr _ -> Some (i, eta_expand gamma tau)
      | _ -> None
      )
    |> List.fold_left (fun acc (i, x) -> Hole_map.add i x acc) Hole_map.empty

(* Solve *)

type solve_result =
  { hole_fillings : (Lang.hole_name * Lang.exp) list list
  ; time_taken : float
  }

let synthesis_pipeline delta sigma assertions =
  assertions
    |> Uneval.simplify_assertions delta sigma
    |> Solve.solve_any delta sigma

let solve_program : Desugar.program -> solve_result response =
  fun program ->
    let (exp, sigma) =
      Desugar.program program
    in
    (* TODO: refactor this part so that double type checking is hopefully not needed, by updating the hole_ctx manually *)
    begin match Type.check sigma Type_ctx.empty exp (Lang.TTuple []) with
      | Error e ->
        Error (TypeError e)

      | Ok delta_original ->
        let pre_pass_hf =
          if !Params.eta_expand
            then eta_expand_all delta_original
            else Hole_map.empty
        in
        let exp =
          Exp.fill_holes pre_pass_hf exp
        in
        begin match Type.check sigma Type_ctx.empty exp (Lang.TTuple []) with
          | Error e ->
              Error (TypeError e)

          | Ok delta ->
              begin match Eval.eval Env.empty exp with
                | Error e ->
                    Error (EvalError e)

                | Ok (_, assertions) ->
                    if !Params.debug_mode then (
                      print_endline "";
                      print_endline "--- ASSERTIONS ---";
                      print_endline "";
                      assertions
                        |> List.iter
                          (fun (r, v) -> print_endline @@ Pretty.res r ^ " == " ^ Pretty.value v);
                      print_endline "";
                    );
                    let () =
                      Term_gen.clear_cache ()
                    in
                    let () =
                      delta
                        |> List.map fst
                        |> List2.maximum
                        |> Option2.with_default 0
                        |> Fresh.set_largest_hole
                    in
                    let () =
                      Uneval.minimal_uneval := true
                    in
                    let
                    ( synthesis_result
                    , time_taken
                    , timed_out
                    ) =
                      let
                      ( minimal_synthesis_result
                      , minimal_time_taken
                      , minimal_timed_out
                      ) =
                        Timer.itimer_timeout "minimal_synthesis_result"
                          !Params.max_total_time
                          (synthesis_pipeline delta sigma) assertions
                          Nondet.none
                      in
                      if
                        minimal_timed_out
                          || not (Nondet.is_empty minimal_synthesis_result)
                      then
                        ( minimal_synthesis_result
                        , minimal_time_taken
                        , minimal_timed_out
                        )
                      else
                        let
                        ( non_minimal_synthesis_result
                        , non_minimal_time_taken
                        , non_minimal_timed_out
                        ) =
                          let () =
                            Uneval.minimal_uneval := false
                          in
                          Timer.itimer_timeout "synthesis_result"
                            (!Params.max_total_time -. minimal_time_taken)
                            (synthesis_pipeline delta sigma) assertions
                            Nondet.none
                        in
                          ( non_minimal_synthesis_result
                          , minimal_time_taken +. non_minimal_time_taken
                          , non_minimal_timed_out
                          )
                    in
                    if timed_out then
                      Error (TimedOut time_taken)
                    else
                      Ok
                        { hole_fillings =
                            synthesis_result
                              |> Nondet.map
                                (fun (hf, _) -> Constraints.merge_solved [pre_pass_hf ; hf])
                              |> Nondet.collapse_option
                              |> Nondet.map (Clean.clean delta_original)
                              |> Nondet.collapse_option
                              |> Nondet.to_list
                        ; time_taken
                        }
              end
        end
    end


let solve ~sketch =
  sketch
    |> parse_program
    |> Result2.and_then solve_program

(* Test *)

type test_result =
  { specification_assertion_count : int
  ; assertion_count : int
  ; top_success : bool
  ; top_recursive_success : bool
  ; time_taken : float
  }

let check :
 Desugar.program ->
 (Lang.hole_name * Lang.exp) list
 -> bool response =
  fun program hole_filling ->
    let (exp_with_holes, sigma) =
      Desugar.program program
    in
    let exp =
      List.fold_left
        ( fun acc binding ->
            Exp.fill_hole binding acc
        )
        exp_with_holes
        hole_filling
    in
    if !Params.debug_mode then (
      print_endline "";
      print_endline "---- FILLINGS ----";
      print_endline "";
      hole_filling
        |> List.iter
          (fun (name, exp) ->
            "?" ^ string_of_int name ^ ": " ^ Pretty.exp exp
            |> print_endline
          );
    );
    match Type.check sigma Type_ctx.empty exp (Lang.TTuple []) with
      | Error e ->
          Error (TypeError e)

      | Ok _ ->
          begin match Eval.eval Env.empty exp with
            | Error _ ->
                Ok false

            | Ok (_, assertions) ->
                Ok (List2.is_empty assertions)
          end

let test_specification ~specification ~sketch ~assertions =
  let open Desugar in
  let open Result2.Syntax in
  let* sketch_program =
    parse_program sketch
  in
  let* { hole_fillings; time_taken } =
    solve_program
      { sketch_program with assertions = assertions }
  in
  let ranked_hole_fillings =
    Rank.sort hole_fillings
  in
  let* top_success =
    ranked_hole_fillings
      |> List2.hd_opt
      |> Option.to_result ~none:NoSolutions
      |> Result2.and_then
           (check { sketch_program with assertions = specification })
  in
  let+ top_recursive_success =
    ranked_hole_fillings
      |> Rank.first_recursive
      |> Option.map
           (check { sketch_program with assertions = specification })
      |> Option2.with_default (Ok false)
  in
  { time_taken
  ; specification_assertion_count =
      List.length specification
  ; assertion_count =
      List.length assertions
  ; top_success
  ; top_recursive_success
  }

let test_assertions ~specification ~sketch ~assertions =
  let open Desugar in
  specification
    |> parse_program
    |> Result.map (fun prog -> prog.assertions)
    |> Result2.and_then
      (fun a -> test_specification ~specification:a ~sketch ~assertions)

let test ~specification ~sketch ~examples =
  let open Desugar in
  examples
    |> parse_program
    |> Result.map
         (fun prog -> prog.assertions)
    |> Result2.and_then
         (fun a -> test_assertions ~specification ~sketch ~assertions:a)

let rec val_to_exp =
  function
  | VTuple vs -> ETuple (List.map val_to_exp vs)
  | VCtor (name, v) -> ECtor (name, [] (* TODO: what about type arguments? *), val_to_exp v) 

let res_to_exp = Res.to_value >> Option.map val_to_exp

let rec args : typ -> typ list * typ =
  function
  | TArr (tau1, tau2) -> Pair2.map_fst (fun ts -> tau1 :: ts) (args tau2) 
  | tau -> [] , tau

let gen_assertions_program ~prog ~model ~size : (exp * exp) Nondet.t =
  let open Desugar in
  let open Nondet.Syntax in
  let* typ, _ =
    Nondet.lift_option @@ List.assoc_opt model prog.definitions
  in
  let types, _ = 
    args typ
  in
  let main_opt =
    Some (app (EVar model) (List.mapi (fun i _ -> EAExp (EHole i)) types))
  in
  let exp, sigma = 
    program { prog with main_opt }
  in
  begin match Eval.eval Env.empty exp with
  | Ok (res, []) ->
    let* inputs =
      Nondet.one_of_each @@ List.map
        (fun t -> Term_gen.up_to I sigma size (Type_ctx.empty, t, None)) types
    in
    let hf =
      List.fold_right (fun (i, x) acc -> Hole_map.add i x acc) 
        (List.mapi Pair2.pair inputs) 
        Hole_map.empty
    in
    begin match Eval.resume hf res with
    | Ok (out, []) -> 
      begin match res_to_exp out with
      | Some exp -> Nondet.pure (app (EVar model) (List.map (fun arg -> EAExp arg) inputs), exp)
      | None -> Nondet.none
      end
    | _ -> Nondet.none
    end
  | _ -> Nondet.none 
  end 

let gen_assertions ~prog ~model ~size : (exp * exp) list response =
  let open Result2.Syntax in
  let* prog =
    parse_program prog
  in
    Ok (gen_assertions_program ~prog ~model ~size |> Nondet.to_list)

(* Assertion Info *)

let extract_arg_list : Lang.exp -> Lang.exp list =
  let rec helper acc =
    function
      | Lang.EApp (_, head, Lang.EAExp arg) ->
          helper (arg :: acc) head

      | _ ->
          acc
  in
  helper []

let assertion_info ~specification ~assertions =
  let open Desugar in
  let open Result2.Syntax in
  let* full_assertions =
    specification
      |> parse_program
      |> Result.map
           (fun prog -> prog.assertions)
  in
  let* partial_assertions =
    assertions
      |> parse_program
      |> Result.map
           (fun prog -> prog.assertions)
  in
  let+ _ =
    Result2.guard PartialNotSubsetFull
      ( List.for_all
          ( fun io ->
              (List.find_opt ((=) io) full_assertions) <> None
          )
          partial_assertions
      )
  in
  List.map
    ( fun (input, output) ->
        ( (List.find_opt ((=) (input, output)) partial_assertions) <> None
        , extract_arg_list (Post_parse.exp input)
        , Post_parse.exp output
        )
    )
    full_assertions
