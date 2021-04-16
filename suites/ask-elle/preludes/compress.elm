type List a
  = Nil ()
  | Cons (a, List a)

append : forall a. List a -> List a -> List a
append <a> l1 l2 =
  case l1 of
    Nil _ ->
      l2
    Cons p ->
      Cons<a> (#2.1 p, append <a> (#2.2 p) l2)
type Nat
  = Z ()
  | S Nat

type Bool
  = F ()
  | T ()

-- TODO: generalize to eq constraints

-- TODO: maybe add foldr & skipDups
-- TODO: maybe add dropWhile
-- TODO: maybe add foldr & skipDups

eq : Nat -> Nat -> Bool
eq n1 n2 =
  case n1 of
    Z _ ->
      case n2 of
        Z _ -> T
        S _ -> F
    S m1 ->
      case n2 of
        Z _ -> F
        S m2 -> eq m1 m2
