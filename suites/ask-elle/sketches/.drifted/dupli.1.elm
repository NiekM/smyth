foldr : forall a. forall b. (a -> b -> b) -> b -> List a -> b
foldr <a> <b> f acc xs =
  case xs of
    Nil _ ->
      acc

    Cons p ->
      f (#2.1 p) (foldr <a> <b> f acc (#2.2 p))

-- NOTE: this definition should be polymorphic in the first type argument of foldr

dupli : forall a. List a -> List a
dupli <a> = foldr <a> <List a -> List a> ?? ?? ??
