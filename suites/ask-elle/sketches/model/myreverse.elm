myreverse : forall a. List a -> List a
myreverse <a> = foldl <List a> <a> (\x y -> Cons<a> (y, x)) []<a>
