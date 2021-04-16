type List a
  = Nil ()
  | Cons (a, List a)

type Bool
  = F ()
  | T ()

type Nat
  = Z ()
  | S Nat

map : forall a. forall b. (a -> b) -> List a -> List b
map <a> <b> f xs =
  case xs of
    Nil _ -> []<b>
    Cons p ->
      Cons<b> (f (#2.1 p), map <a> <b> f (#2.2 p))

isEven : Nat -> Bool
isEven n =
  case n of
    Z _  -> T ()
    S m1 ->
      case m1 of
        Z _  -> F ()
        S m2 -> isEven m2
