myreverse : NatList -> NatList
myreverse = foldl <NatList> (\x y -> Cons (y, x)) []
