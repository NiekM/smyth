elementat : forall a. List a -> Nat -> Maybe a
elementat <a> xs n =
  case xs of
    Nil _ -> Nothing<a>
    Cons p ->
      case n of
        Z _ -> Just<a> (#2.1 p)
        S m -> elementat <a> (#2.2 p) m
