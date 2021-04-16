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
