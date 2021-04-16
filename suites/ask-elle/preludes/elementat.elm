type List a
  = Nil ()
  | Cons (a, List a)

type Nat
  = Z ()
  | S Nat

type Maybe a
  = Nothing ()
  | Just a

-- TODO: partial function

-- NOTE: 0-based, unlike Ask-Elle, which is 1-based in accordance with H99

-- TODO: maybe add index
-- TODO: maybe add predecessor
