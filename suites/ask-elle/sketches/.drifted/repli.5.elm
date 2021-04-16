-- TODO: why does this work at all?

repli : forall a. List a -> Nat -> List a
repli <a> xs n = concatMap <List a> <a> ?? (replicate <List a> n xs)
