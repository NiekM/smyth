type List a
  = Nil ()
  | Cons (a, List a)

type Bool
  = F ()
  | T ()

type Nat
  = Z ()
  | S Nat
 
-- NOTE: these are only for testing

isEven : Nat -> Bool
isEven n =
  case n of
    Z _  -> T ()
    S m1 ->
      case m1 of
        Z _  -> F ()
        S m2 -> isEven m2

isNonzero : Nat -> Bool
isNonzero n =
  case n of
    Z _ -> F ()
    S _ -> T ()
