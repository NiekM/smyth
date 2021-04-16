compress : List Nat -> List Nat
compress xs =
  case xs of
    Nil _ -> []<Nat>
    Cons p ->
      case #2.2 p of
        Nil _ -> xs
        Cons q ->
          append <Nat> 
            (
              case eq (#2.1 p) (#2.1 q) of
                F _ -> [#2.1 p]<Nat>
                T _ -> []<Nat>
            ) 
            (compress (#2.2 p))
