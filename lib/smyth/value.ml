open Lang

let rec from_exp_opt =
  function
  | ETuple es ->
    List.map from_exp_opt es
      |> Option2.sequence
      |> Option.map (fun vs -> VTuple vs)

  | ECtor (name, _, e) ->
    from_exp_opt e
      |> Option.map (fun v -> VCtor (name, v))

  | _ ->
    None

let rec from_res_opt =
  function
  | RTuple rs ->
    List.map from_res_opt rs
      |> Option2.sequence
      |> Option.map (fun vs -> VTuple vs)

  | RCtor (name, r) ->
    from_res_opt r
      |> Option.map (fun v -> VCtor (name, v))

  | _ ->
    None


let rec from_example_opt =
  function
  | ExTuple es ->
    List.map from_example_opt es
      |> Option2.sequence
      |> Option.map (fun vs -> VTuple vs)

  | ExCtor (name, e) ->
    from_example_opt e
      |> Option.map (fun v -> VCtor (name, v))

  | _ ->
    None
