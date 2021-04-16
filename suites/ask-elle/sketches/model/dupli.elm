dupli : forall a. List a -> List a
dupli <a> xs = concatMap <a> <a> (\x -> [x, x]<a>) xs 
