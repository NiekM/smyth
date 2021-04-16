foldr : forall a. forall b. (a -> b -> b) -> b -> List a -> b
foldr <a> <b> f acc xs =
  case xs of
    Nil _ ->
      acc

    Cons p ->
      f (#2.1 p) (foldr <a> <b> f acc (#2.2 p))

mylast : forall a. List a -> Maybe a
mylast <a> xs = foldr <a> <Maybe a> ?? ?? ??
