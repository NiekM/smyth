reverse : NatList -> NatList
reverse = foldl <NatList> (\x y -> Cons (y, x)) []
