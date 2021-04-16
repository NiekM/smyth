rotate : forall a. List a -> Nat -> List a
rotate <a> xs n =
  case n of
    Z _ -> xs
    S m ->
      case xs of
        Nil _ -> xs
        Cons p ->
          rotate <a> (append <a> (#2.2 p) [#2.1 p]<a>) m
