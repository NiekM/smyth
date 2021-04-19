app : forall a. forall b. (a -> b) -> a -> b
app <a> <b> f x = f x

encode : List Nat -> List (Nat, Nat) 
encode xs = app <List Nat> <List (Nat, Nat)> ?? xs
