myconcat : forall a. List (List a) -> List a
myconcat <a> = foldr <List a> <List a> (append <a>) ([]<a>)
