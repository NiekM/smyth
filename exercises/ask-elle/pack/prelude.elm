type Nat
  = Z ()
  | S Nat

type List a
  = Nil ()
  | Cons (a, List a)

type Cmp
  = LT ()
  | EQ ()
  | GT ()

append : forall a. List a -> List a -> List a
append l1 l2 =
  case l1 of
    Nil _ ->
      l2
    Cons p ->
      Cons (#2.1 p, append (#2.2 p) l2)

foldr : forall a. forall b. (a -> b -> b) -> b -> List a -> b
foldr <a> f acc xs =
  case xs of
    Nil _ ->
      acc

    Cons p ->
      f (#2.1 p) (foldr <a> f acc (#2.2 p))
