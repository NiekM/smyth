-- NOTE: this definition should be polymorphic in the first type argument of foldr
-- NOTE: segmentation fault (core dumped)

compress : List Nat -> List Nat
compress = foldr <Nat> <List Nat -> List Nat> ?? ?? ??
