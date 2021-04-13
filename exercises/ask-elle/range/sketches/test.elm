range : Nat -> Nat -> NatList
range x y =
  case compare x y of
    LT _ -> ??
    EQ _ -> [x]
    GT _ -> []
