myconcat : List (List Nat) -> List Nat
myconcat = foldr <List Nat> <List Nat> (append <Nat>) ([]<Nat>)
