-- TODO: why does it get stuck on eta-expansion? probably due to skipping assertions
-- should be ?0 = replicate <a> n

repli : forall a. List a -> Nat -> List a
repli <a> xs n =
  let
    f : a -> List a
    f = ??
  in
    concatMap <a> <a> f xs
