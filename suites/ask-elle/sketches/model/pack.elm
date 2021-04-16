helper : Nat -> List (List Nat) -> List (List Nat)
helper x xss =
  case xss of
    Nil _ -> [[x]<Nat>]<List Nat>
    Cons p ->
      case (#2.1 p) of
        Nil _ -> []<List Nat>
        Cons q ->
          case eq x (#2.1 q) of
            F _ -> Cons<List Nat> ([x]<Nat>, xss)
            T _ -> Cons<List Nat> (Cons<Nat> (x, #2.1 p), #2.2 p)

pack : List Nat -> List (List Nat)
pack = foldr <Nat> <List (List Nat)> helper []<List Nat>
