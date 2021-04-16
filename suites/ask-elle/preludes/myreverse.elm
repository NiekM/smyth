type List a
  = Nil ()
  | Cons (a, List a)

append : forall a. List a -> List a -> List a
append <a> l1 l2 =
  case l1 of
    Nil _ ->
      l2
    Cons p ->
      Cons<a> (#2.1 p, append <a> (#2.2 p) l2)

-- TODO: maybe add flip & cons

foldl : forall a. forall b. (a -> b -> a) -> a -> List b -> a
foldl <a> <b> f acc xs =
  case xs of
    Nil _ ->
      acc

    Cons p ->
      foldl <a> <b> f (f acc (#2.1 p)) (#2.2 p)
