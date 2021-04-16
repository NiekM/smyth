type List a
  = Nil ()
  | Cons (a, List a)

type Nat
  = Z ()
  | S Nat

-- TODO: add replicate & append & subtract
-- TODO: add unfoldr & range
-- TODO: add take & iterate & repeat

map : forall a. forall b. (a -> b) -> List a -> List b
map <a> <b> f xs =
  case xs of
    Nil _ -> []<b>
    Cons p ->
      Cons<b> (f (#2.1 p), map <a> <b> f (#2.2 p))
