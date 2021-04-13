type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

type NatListList
  = LNil ()
  | LCons (NatList, NatListList)

append : NatList -> NatList -> NatList
append l1 l2 =
  case l1 of
    Nil _ ->
      l2
    Cons p ->
      Cons (#2.1 p, append (#2.2 p) l2)

foldr : forall a. (NatList -> a -> a) -> a -> NatListList -> a
foldr <a> f acc xs =
  case xs of
    LNil _ ->
      acc

    LCons p ->
      f (#2.1 p) (foldr <a> f acc (#2.2 p))
