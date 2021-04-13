type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

map : (Nat -> Nat) -> NatList -> NatList
map f xs =
  case xs of
    Nil _ -> []
    Cons p -> Cons (f (#2.1 p), map f (#2.2 p))
 
 -- TODO: polymorphic/boolean list 