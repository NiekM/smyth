type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

foldl : forall a. (a -> Nat -> a) -> a -> NatList -> a
foldl <a> f acc xs =
  case xs of
    Nil _ ->
      acc

    Cons p ->
      foldl <a> f (f acc (#2.1 p)) (#2.2 p)
