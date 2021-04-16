allpairs : forall a. List a -> List (a, a)
allpairs <a> xs =
  concatMap <a> <(a, a)> (\x -> map <a> <(a, a)> (\y -> (x, y)) xs) xs
