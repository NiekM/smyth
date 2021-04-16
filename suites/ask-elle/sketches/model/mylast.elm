mylast : forall a. List a -> Maybe a
mylast <a> xs =
  case xs of
    Nil _ -> Nothing<a>
    Cons p ->
      case #2.2 p of
        Nil _ -> Just<a> (#2.1 p)
        Cons q ->
          mylast <a> (#2.2 p)
