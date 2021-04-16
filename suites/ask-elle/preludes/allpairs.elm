type List a
  = Nil ()
  | Cons (a, List a)

-- TODO: order of outputs should not matter

-- TODO: maybe add zip & repeat (NOTE: lazy)

map : forall a. forall b. (a -> b) -> List a -> List b
map <a> <b> f xs =
  case xs of
    Nil _ -> []<b>
    Cons p ->
      Cons<b> (f (#2.1 p), map <a> <b> f (#2.2 p))

append : forall a. List a -> List a -> List a
append <a> l1 l2 =
  case l1 of
    Nil _ ->
      l2
    Cons p ->
      Cons<a> (#2.1 p, append <a> (#2.2 p) l2)

concatMap : forall a. forall b. (a -> List b) -> List a -> List b
concatMap <a> <b> f xs =
  case xs of
    Nil _ -> []<b>
    Cons p ->
      append<b> (f (#2.1 p)) (concatMap <a> <b> f (#2.2 p))
