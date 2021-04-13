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

concat : NatList -> NatList -> NatList
concat xs ys = foldr <NatList> (\x xs -> Cons (x, xs)) ys xs

map : (Nat -> Nat) -> NatList -> NatList
map f xs =
  case xs of
    Nil _ -> []
    Cons p -> Cons (f (#2.1 p), map f (#2.2 p))

concatMap : (Nat -> NatList) -> NatList -> NatList
concatMap f xs =
  case xs of
    Nil _ -> []
    Cons p -> concat (f (#2.1 p)) (concatMap f (#2.2 p))
