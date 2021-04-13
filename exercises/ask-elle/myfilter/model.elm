filter : (Nat -> Bool) -> NatList -> NatList
filter f = 
  foldr <NatList> 
    (\x xs ->
      case f x of
        F _ -> xs
        T _ -> Cons (x, xs)
    )
    []
