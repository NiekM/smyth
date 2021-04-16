type List a
  = Nil ()
  | Cons (a, List a)

-- TODO: partial function?

-- TODO: maybe add foldr & zipWith & repeat (NOTE: lazy)
-- TODO: maybe add index & length & compare
-- TODO: maybe add map & zipWith
