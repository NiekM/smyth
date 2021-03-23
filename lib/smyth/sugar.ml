open Lang

let rec vnat : value -> int option =
  function
    | VCtor ("S", arg) ->
      Option.map ((+) 1) (vnat arg)

    | VCtor ("Z", VTuple []) ->
      Some 0

    | _ ->
      None

let rec vlist : value -> value list option =
  function
    | VCtor ("Cons", VTuple [head; tail]) ->
      vlist tail
        |> Option.map (fun t -> head :: t)

    | VCtor ("Nil", VTuple []) ->
      Some []

    | _ ->
      None

let nat : exp -> int option =
  Option2.and_then vnat << Value.from_exp_opt

let listt : exp -> (exp list * typ list) option =
  let rec helper expected_opt =
    function
      | ECtor ("Cons", type_args, ETuple [head; tail]) ->
          let good () =
            Option.map
              (fun (es, taus) -> (head :: es, taus))
              (helper expected_opt tail)
          in
          begin match expected_opt with
            | Some expected ->
                if Type.equal (TTuple expected) (TTuple type_args) then
                  good ()

                else
                  None

            | None ->
                good ()
          end

      | ECtor ("Nil", type_args, ETuple []) ->
          let good () =
            Some ([], type_args)
          in
          begin match expected_opt with
            | Some expected ->
                if Type.equal (TTuple expected) (TTuple type_args) then
                  good ()
                else
                  None

            | None ->
                good ()
          end

      | _ ->
          None
  in
  helper None
