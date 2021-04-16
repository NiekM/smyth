type List a
  = Nil ()
  | Cons (a, List a)

type Bool
  = F ()
  | T ()

type Nat
  = Z ()
  | S Nat

-- TODO: laziness is not supported

-- TODO: maybe add filter & mod & eq & range
-- TODO: maybe add filter & rem & range
-- TODO: maybe add iterate & map & range
