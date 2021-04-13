range : Nat -> Nat -> NatList
range x y = Cons (x, range (S x) y) 
