type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

foldr : forall a. (Nat -> a -> a) -> a -> NatList -> a
foldr <a> f acc xs =
  case xs of
    Nil _ ->
      acc

    Cons p ->
      f (#2.1 p) (foldr <a> f acc (#2.2 p))
