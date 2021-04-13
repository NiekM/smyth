range : Nat -> Nat -> NatList
range x y =
  case compare x y of
    LT _ -> Cons (??, range (S ??) y)
    EQ _ -> [??]
    GT _ -> []
