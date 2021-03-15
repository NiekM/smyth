type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

type Cmp
  = LT ()
  | EQ ()
  | GT ()

foldr : (Nat -> NatList -> NatList) -> NatList -> NatList -> NatList
foldr f acc xs =
  case xs of
    Nil _ ->
      acc

    Cons p ->
      f (#2.1 p) (foldr f acc (#2.2 p))

--foldr : forall a. (Nat -> a -> a) -> a -> NatList -> a
--foldr <a> f acc xs =
--  case xs of
--    Nil _ ->
--      acc
--
--    Cons p ->
--      f (#2.1 p) (foldr <a> f acc (#2.2 p))

compare : Nat -> Nat -> Cmp
compare n1 n2 =
  case n1 of
    Z _ ->
      case n2 of
        Z _ -> EQ ()
        S _ -> LT ()
    S m1 ->
      case n2 of
        Z _  -> GT ()
        S m2 -> compare m1 m2
 