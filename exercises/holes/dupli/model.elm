dupli : NatList -> NatList
dupli = concatMap (\x -> [x, x])
