type List a
  = Nil ()
  | Cons (a, List a)

type Nat
  = Z ()
  | S Nat

-- NOTE: 0-based, unlike Ask-Elle, which is 1-based in accordance with H99

-- TODO: maybe add drop & add
-- TODO: maybe add reverse
-- TODO: maybe add compare
-- TODO: maybe add splitAt
-- TODO: maybe add map & filter & zip & fromTo & compare
-- TODO: maybe add splitAt 

take : forall a. Nat -> List a -> List a
take <a> n xs =
  case n of
    Z _ -> []<a>
    S m ->
      case xs of
        Nil _ -> xs
        Cons p ->
          Cons<a> (#2.1 p, take <a> m (#2.2 p))
