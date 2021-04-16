palindrome : List Nat -> Bool
palindrome xs = eqList xs (reverse <Nat> xs)