type List a
  = Nil ()
  | Cons (a, List a)

type Nat
  = Z ()
  | S Nat

type Cmp
  = LT ()
  | EQ ()
  | GT ()

-- TODO: generalize to ord constraints

-- TODO: maybe add fromTo
-- TODO: maybe add take & subtract & from (NOTE: lazy)
-- TODO: maybe add take & subtract & iterate (NOTE: lazy)
-- TODO: maybe add unfoldr

compare : Nat -> Nat -> Cmp
compare n1 n2 =
  case n1 of
    Z _ ->
      case n2 of
        Z _ -> EQ ()
        S _ -> LT ()
    S m1 ->
      case n2 of
        Z _  -> GT ()
        S m2 -> compare m1 m2

type Bool
  = F ()
  | T ()

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
 
take : forall a. Nat -> List a -> List a
take <a> n xs =
  case n of
    Z _ -> []<a>
    S m ->
      case xs of
        Nil _ -> xs
        Cons p ->
          Cons<a> (#2.1 p, take <a> m (#2.2 p))
 
add : Nat -> Nat -> Nat
add n1 n2 =
  case n1 of
    Z _ -> n2
    S m1 -> S (add m1 n2)

sub : Nat -> Nat -> Nat
sub n1 n2 =
  case n1 of
    Z _ -> 0
    S m1 ->
      case n2 of
        Z _ -> n1
        S m2 -> sub m1 m2
