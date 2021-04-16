foldr : forall a. forall b. (a -> b -> b) -> b -> List a -> b
foldr <a> <b> f acc xs =
  case xs of
    Nil _ ->
      acc

    Cons p ->
      f (#2.1 p) (foldr <a> <b> f acc (#2.2 p))

myfilter : forall a. (a -> Bool) -> List a -> List a
myfilter <a> f = 
  foldr <a> <List a> 
    (\x xs ->
      case f x of
        F _ -> xs
        T _ -> Cons<a> (x, xs)
    )
    []<a>
