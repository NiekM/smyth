mylength : NatList -> Nat
mylength = foldr <Nat> (\x y -> S y) 0
