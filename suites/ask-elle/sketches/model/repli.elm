repli : forall a. List a -> Nat -> List a
repli <a> xs n = concatMap <a> <a> (replicate <a> n) xs 
