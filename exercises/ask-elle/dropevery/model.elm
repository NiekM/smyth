-- NOTE: this is incorrect!
dropevery : NatList -> Nat -> NatList
dropevery xs n =
  case n of
    Z _ -> []
    S n ->
      let
        helper : NatList -> Nat -> NatList
        helper ys m =
          case ys of
            Nil _ -> []
            Cons p ->
              case #2.1 p of
                Z _ -> helper (#2.2 p) n
                S k -> Cons (#2.1 p, helper (#2.2 p) k)
      in
        helper xs n
