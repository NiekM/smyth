elementat : NatList -> Nat -> Nat
elementat xs n =
  case n of
    Z _ ->
      case xs of
        Nil _ -> 0
        Cons p -> #2.1 p
    S n ->
      case xs of
        Nil _ -> 0
        Cons p ->
          elementat (#2.2 p) n
