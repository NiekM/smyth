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

cons_ : Nat -> NatList -> NatList
cons_ x xs = 
  case xs of
    Nil _ -> [x]
    Cons p -> 
      case compare x (#2.1 p) of
        LT _ -> Cons (x, xs)
        EQ _ -> xs
        GT _ -> Cons (x, xs)

compress : NatList -> NatList
compress xs = 
  case xs of
    Nil _ -> []
    Cons p -> ??
