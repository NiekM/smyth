-- NOTE: specifying foldl is enough to synthesize myreverse

myreverse : forall a. List a -> List a
myreverse <a> = foldl <List a> <a> ?? ??
