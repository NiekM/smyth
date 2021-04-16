type List a
  = Nil ()
  | Cons (a, List a)

type Maybe a
  = Nothing ()
  | Just a

type Nat
  = Z ()
  | S Nat

-- NOTE: 0-based, unlike Ask-Elle, which is 1-based in accordance with H99

-- TODO: maybe add leq & length & index & append & take & drop
-- TODO: maybe add splitAt & append
