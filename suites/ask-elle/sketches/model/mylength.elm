mylength : forall a. List a -> Nat
mylength <a> xs =
  case xs of
    Nil _ -> 0
    Cons p ->
      S (mylength <a> (#2.2 p))
