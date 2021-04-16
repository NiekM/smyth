split : forall a. List a -> Nat -> (List a, List a)
split <a> xs n =
  case n of
    Z _ -> ([]<a>, xs)
    S m ->
      case xs of
        Nil _ -> ([]<a>, []<a>)
        Cons p ->
          let
            r : (List a, List a)
            r = split <a> (#2.2 p) m
          in
            (Cons<a> (#2.1 p, #2.1 r), #2.2 r)
