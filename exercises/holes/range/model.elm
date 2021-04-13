range : Nat -> Nat -> NatList
range x y =
  case compare x y of
    LT _ -> Cons (x, range (S x) y)
    EQ _ -> [x]
    GT _ -> []
