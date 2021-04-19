-- TODO: why does it get stuck on eta-expansion? probably due to skipping assertions
-- should be ?0 = replicate <a> n, ?1 = xs

repli : forall a. List a -> Nat -> List a
repli <a> xs n = concatMap <a> <a> ?? ??
