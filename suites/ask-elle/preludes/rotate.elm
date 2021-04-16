type List a
  = Nil ()
  | Cons (a, List a)

type Nat
  = Z ()
  | S Nat

-- TODO: fix recursion on later arguments
-- TODO: allow for negative inputs

-- TODO: maybe add mod & take & drop & cycle (NOTE: lazy)
-- TODO: maybe add take & drop & mod & length
-- TODO: maybe add subtract & add & compare & splitAt & length

append : forall a. List a -> List a -> List a
append <a> l1 l2 =
  case l1 of
    Nil _ ->
      l2
    Cons p ->
      Cons<a> (#2.1 p, append <a> (#2.2 p) l2)
 