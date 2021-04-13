type Nat
  = Z ()
  | S Nat

type Bool
  = F ()
  | T ()

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

even : Nat -> Bool
even x =
  case x of
    Z _ -> T ()
    S y ->
      case y of
        Z _ -> F ()
        S z -> even z

isZero : Nat -> Bool
isZero x =
  case x of
    Z _ -> T ()
    S _ -> F ()
