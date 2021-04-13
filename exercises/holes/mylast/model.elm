mylast : NatList -> Nat
mylast xs =
  case xs of
    Nil _ -> 0
    Cons p ->
      case #2.2 p of
        Nil _ -> #2.1 p
        Cons _ -> mylast (#2.2 p)
 