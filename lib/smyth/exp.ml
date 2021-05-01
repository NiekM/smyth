open Lang

let rec syntactically_equal e1 e2 =
  match (e1, e2) with
    | (EFix (mf1, x1, body1), EFix (mf2, x2, body2)) ->
        let mf_equal =
          begin match (mf1, mf2) with
            | (Some f1, Some f2) ->
                String.equal f1 f2

            | (None, None) ->
                true

            | _ ->
                false
          end
        in
        let x_equal =
          match (x1, x2) with
            | (PatParam p1, PatParam p2) ->
                Pat.syntactically_equal p1 p2

            | (TypeParam t1, TypeParam t2) ->
                String.equal t1 t2

            | _ ->
                false
        in
          mf_equal
            && x_equal
            && syntactically_equal body1 body2

    | (EApp (b1, head1, EAExp arg1), EApp (b2, head2, EAExp arg2)) ->
        Bool.equal b1 b2
          && syntactically_equal head1 head2
          && syntactically_equal arg1 arg2

    | (EApp (b1, e1, EAType t1), EApp (b2, e2, EAType t2)) ->
        Bool.equal b1 b2
          && syntactically_equal e1 e2
          && Type.equal t1 t2

    | (EVar x1, EVar x2) ->
        String.equal x1 x2

    | (ETuple es1, ETuple es2) ->
        Int.equal (List.length es1) (List.length es2)
          && List.for_all2 syntactically_equal es1 es2

    | (EProj (n1, i1, arg1), EProj (n2, i2, arg2)) ->
        Int.equal n1 n2
          && Int.equal i1 i2
          && syntactically_equal arg1 arg2

    | (ECtor (name1, taus1, arg1), ECtor (name2, taus2, arg2)) ->
        String.equal name1 name2
          && Int.equal (List.length taus1) (List.length taus2)
          && List.for_all2 Type.equal taus1 taus2
          && syntactically_equal arg1 arg2

    | (ECase (s1, branches1), ECase (s2, branches2)) ->
        syntactically_equal s1 s2
          && Int.equal (List.length branches1) (List.length branches2)
          && List.for_all2
               ( fun (ctor1, (arg1, body1)) (ctor2, (arg2, body2)) ->
                   String.equal ctor1 ctor2
                     && Pat.syntactically_equal arg1 arg2
                     && syntactically_equal body1 body2
               )
               branches1
               branches2

    | (EHole name1, EHole name2) ->
        Int.equal name1 name2

    | (EAssert (left1, right1), EAssert (left2, right2)) ->
        syntactically_equal left1 left2
          && syntactically_equal right1 right2

    | (ETypeAnnotation (e1, tau1), ETypeAnnotation (e2, tau2)) ->
        Type.equal tau1 tau2
          && syntactically_equal e1 e2

    | _ ->
        false

let rec largest_hole : exp -> hole_name =
  fun exp ->
    match exp with
      (* Main case *)

      | EHole hole_name ->
          hole_name

      (* Other cases *)

      | EApp (_, e1, EAExp e2)
      | EAssert (e1, e2) ->
          max (largest_hole e1) (largest_hole e2)

      | EFix (_, _, e)
      | EApp (_, e, EAType _)
      | EProj (_, _, e)
      | ECtor (_, _, e)
      | ETypeAnnotation (e, _) ->
          largest_hole e

      | EVar _ ->
          Fresh.unused

      | ETuple components ->
          components
            |> List.map largest_hole
            |> List2.maximum
            |> Option2.with_default Fresh.unused

      | ECase (scrutinee, branches) ->
          let branch_max =
            branches
              |> List.map (snd >> snd >> largest_hole)
              |> List2.maximum
              |> Option2.with_default Fresh.unused
          in
          max (largest_hole scrutinee) branch_max

let rec has_special_recursion : exp -> bool =
  function
    | EFix (_, _, body) ->
        has_special_recursion body

    | EApp (special, e1, EAExp e2) ->
        special || has_special_recursion e1 || has_special_recursion e2

    | EApp (special, e1, EAType _) ->
        special || has_special_recursion e1

    | EVar _ ->
        false

    | ETuple components ->
        List.exists has_special_recursion components

    | EProj (_, _, arg) ->
        has_special_recursion arg

    | ECtor (_, _, arg) ->
        has_special_recursion arg

    | ECase (scrutinee, branches) ->
        has_special_recursion scrutinee
          || List.exists (fun (_, (_, e)) -> has_special_recursion e) branches

    | EHole _ ->
        false

    | EAssert (e1, e2) ->
        has_special_recursion e1 || has_special_recursion e2

    | ETypeAnnotation (e, _) ->
        has_special_recursion e

(* TODO: fill_holes should probably subsume fill_hole *)
(* TODO: probably should first clean/propagate hf before calling fill_holes *)
let fill_holes (hf : hole_filling) : exp -> exp =
  let rec helper : exp -> exp =
    function
      (* Main case *)

      | EHole hole_name ->
        hf
          |> Hole_map.find_opt hole_name
          |> Option2.with_default (EHole hole_name)

      (* Other cases *)

      | EFix (f, x, body) ->
          EFix (f, x, helper body)

      | EApp (special, e1, EAExp e2) ->
          EApp (special, helper e1, EAExp (helper e2))

      | EApp (special, e1, EAType type_arg) ->
          EApp (special, helper e1, EAType type_arg)

      | EVar x ->
          EVar x

      | ETuple components ->
          ETuple (List.map helper components)

      | EProj (n, i, arg) ->
          EProj (n, i, helper arg)

      | ECtor (ctor_name, type_args, arg) ->
          ECtor (ctor_name, type_args, helper arg)

      | ECase (scrutinee, branches) ->
          ECase
            ( helper scrutinee
            , List.map (Pair2.map_snd (Pair2.map_snd helper)) branches
            )

      | EAssert (e1, e2) ->
          EAssert (helper e1, helper e2)

      | ETypeAnnotation (e, tau) ->
          ETypeAnnotation (helper e, tau)
  in
  helper

let rec sub_sketches : exp -> exp Nondet.t =
  fun exp ->
    match exp with
    | EHole _
    | EAssert _ -> Nondet.pure exp
    | _ ->
      Nondet.union
        [ non_hole_sub_sketches exp
        ; Nondet.pure (EHole (Fresh.gen_hole ()))
        ]

and non_hole_sub_sketches : exp -> exp Nondet.t =
  let open Nondet.Syntax in
  function
    | EFix (f, x, body) ->
      let* body =
        sub_sketches body
      in
        Nondet.pure @@ EFix (f, x, body)
    | EApp (special, head, EAExp arg) ->
      let* head =
        non_hole_sub_sketches head
      in
      let* arg =
        sub_sketches arg
      in
        Nondet.pure @@ EApp (special, head, EAExp arg)
    | EApp (special, head, arg) ->
      let* head =
        non_hole_sub_sketches head
      in
        Nondet.pure @@ EApp (special, head, arg)
    | EVar name -> Nondet.pure @@ EVar name
    | ETuple es ->
      let* es =
        Nondet.one_of_each @@ List.map sub_sketches es
      in
        Nondet.pure @@ ETuple es
    | EProj (n, i, arg) ->
    (* TODO: does it make sense to have a projection of a hole? *)
      let* arg =
        sub_sketches arg
      in
        Nondet.pure @@ EProj (n, i, arg)
    | ECtor (name, ts, arg) ->
      let* arg =
        sub_sketches arg
      in
        Nondet.pure @@ ECtor (name, ts, arg)
    | ECase (scrutinee, branches) ->
      let* scrutinee =
        sub_sketches scrutinee
      in
      let* branches =
        branches
          |> List.map
            (fun (s, (p, e)) -> Nondet.map (fun x -> (s, (p, x))) (sub_sketches e))
          |> Nondet.one_of_each
      in
        Nondet.pure @@ ECase (scrutinee, branches)
    | ETypeAnnotation (exp, typ) ->
      let* exp =
        sub_sketches exp
      in
        Nondet.pure @@ ETypeAnnotation (exp, typ)
    | EHole _
    (* TODO: check if this makes sense: no strict subsketches for assertions, right? *)
    | EAssert _ -> Nondet.none

let rec sub_expressions : exp -> exp Nondet.t =
  fun exp ->
    Nondet.union
      [ Nondet.pure exp
      ; strict_sub_expressions exp
      ]

and strict_sub_expressions : exp -> exp Nondet.t =
  function
  | EFix (_, _, exp)
  | EApp (_, exp, EAType _)
  | EProj (_, _, exp)
  | ECtor (_, _, exp)
  | ETypeAnnotation (exp, _) ->
    sub_expressions exp

  | EApp (_, exp1, EAExp exp2)
  | EAssert (exp1, exp2) ->
    [exp1; exp2]
      |> List.map sub_expressions
      |> Nondet.union

  | ETuple es ->
    es
      |> List.map sub_expressions
      |> Nondet.one_of_each
      |> Nondet.map (fun xs -> ETuple xs)

  | ECase (scrutinee, branches) ->
    scrutinee :: List.map (snd >> snd) branches
        |> List.map sub_expressions
        |> Nondet.union

  | _ -> Nondet.none

let subst : Type.subst -> exp -> exp =
  fun th ->
    let rec go =
      function
      | EFix (f, p, e) ->
        EFix (f, p, go e)

      | EApp (s, e1, EAExp e2) ->
        EApp (s, go e1, EAExp (go e2))

      | EApp (s, e, EAType t) ->
        EApp (s, go e, EAType (Type.subst th t))

      | EVar a ->
        EVar a

      | ETuple components ->
        ETuple (List.map go components)

      | EProj (n, i, e) ->
        EProj (n, i, go e)

      | ECtor (s, ts, e) ->
        ECtor (s, List.map (Type.subst th) ts, go e)

      | ECase (scrutinee, branches) ->
        ECase (scrutinee, branches |> List.map (fun (s, (p, e)) -> s, (p, go e)))

      | EHole hole_name ->
        EHole hole_name

      | EAssert (e1, e2) ->
        EAssert (go e1, go e2)

      | ETypeAnnotation (e, t) ->
        ETypeAnnotation (go e, Type.subst th t)

    in go
