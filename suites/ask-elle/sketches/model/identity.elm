insertAt : forall a. Nat -> a -> List a -> List a
insertAt <a> n x xs =
  case n of
    Z _ -> Cons<a> (x, xs)
    S m ->
      case xs of
        Nil _ -> [x]<a>
        Cons p ->
          Cons<a> (#2.1 p, insertAt <a> m x (#2.2 p))

type Cmp
  = LT ()
  | EQ ()
  | GT ()

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
 
range : Nat -> Nat -> List Nat
range x y =
  case compare x y of
    LT _ -> Cons<Nat> (x, range (S x) y)
    EQ _ -> [x]<Nat>
    GT _ -> []<Nat>

replicate : forall a. Nat -> a -> List a
replicate <a> n x =
  case n of
    Z _ -> []<a>
    S m -> Cons<a> (x, replicate <a> m x)

identity : Nat -> List (List Nat)
identity n =
  case n of
    Z _ -> []<List Nat>
    S m ->
      map <Nat> <List Nat> 
        (\x -> insertAt <Nat> x 1 (replicate <Nat> m 0))
        (range 0 m)
