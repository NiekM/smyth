f : Nat -> List (Nat, Nat) -> List (Nat, Nat)
f x xs =
  case xs of
    Nil _ -> [(1, x)]<(Nat, Nat)>
    Cons p ->
      case eq x (#2.2 (#2.1 p)) of
        F _ -> Cons<(Nat,Nat)> ((1, x), xs)
        T _ -> Cons<(Nat,Nat)> ((S (#2.1 (#2.1 p)), #2.2 (#2.1 p)), #2.2 p)

encode : List Nat -> List (Nat, Nat) 
encode = foldr <Nat> <List (Nat, Nat)> f []<(Nat, Nat)>
