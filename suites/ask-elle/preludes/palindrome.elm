type List a
  = Nil ()
  | Cons (a, List a)

type Nat
  = Z ()
  | S Nat

type Bool
  = F ()
  | T ()

-- TODO: generalize to eq constraints

-- TODO: maybe add head & last & init & tail

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
 
eqList : List Nat -> List Nat -> Bool
eqList xs ys =
  case xs of
    Nil _ ->
      case ys of
        Nil _ -> T
        Cons _ -> F
    Cons p ->
      case ys of
        Nil _ -> F
        Cons q ->
          case eq (#2.1 p) (#2.1 q) of
            F _ -> F
            T _ ->
              eqList (#2.2 p) (#2.2 q)

foldl : forall a. forall b. (a -> b -> a) -> a -> List b -> a
foldl <a> <b> f acc xs =
  case xs of
    Nil _ ->
      acc

    Cons p ->
      foldl <a> <b> f (f acc (#2.1 p)) (#2.2 p)

reverse : forall a. List a -> List a
reverse <a> = foldl <List a> <a> (\xs x -> Cons<a> (x, xs)) []<a>
