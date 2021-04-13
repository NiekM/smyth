length : List Nat -> Nat
length = foldr <Nat> <Nat> (\x y -> S y) 0
