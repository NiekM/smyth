filter : (Nat -> Bool) -> NatList -> NatList
filter f xs =
  case xs of
    Nil _ -> ??
    Cons p ->
      case f (#2.1 p) of
        F _ -> ??
        T _ -> ?? 
