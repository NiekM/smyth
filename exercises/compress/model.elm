skip_dups : Nat -> NatList -> NatList 
skip_dups x xs =
  case xs of
    Nil _ -> [x]
    Cons p ->
      case compare x (#2.1 p) of
        LT _ -> Cons (x, Cons p)
        EQ _ -> Cons p 
        GT _ -> Cons (x, Cons p)

compress : NatList -> NatList
compress = foldr skip_dups []
