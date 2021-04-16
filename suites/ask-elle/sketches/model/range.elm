range : Nat -> Nat -> List Nat
range x y =
  case compare x y of
    -- NOTE: this solution cannot be discovered because
    --       the arguments are not structurally smaller
    LT _ -> Cons<Nat> (x, range (S x) y)
    EQ _ -> [x]<Nat>
    GT _ -> []<Nat>
