removeat : forall a. Nat -> List a -> (Maybe a, List a)
removeat <a> n xs =
  case n of
    Z _ ->
      case xs of
        Nil _ -> (Nothing<a>, []<a>)
        Cons p ->
          (Just<a> (#2.1 p), #2.2 p)
    S m ->
      case xs of
        Nil _ -> (Nothing<a>, []<a>)
        Cons p ->
          let
            r : (Maybe a, List a)
            r = removeat <a> m (#2.2 p)
          in
            (#2.1 r, Cons<a> (#2.1 p, #2.2 r))
