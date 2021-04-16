slice : forall a. List a -> Nat -> Nat -> List a
slice <a> xs n1 n2 =
  case n1 of
    Z _ -> take <a> (S n2) xs
    S m1 ->
      case n2 of
        Z _ -> []<a>
        S m2 ->
          case xs of
            Nil _ -> []<a>
            Cons p -> slice <a> (#2.2 p) m1 m2
