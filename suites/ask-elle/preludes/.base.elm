type List a
  = Nil ()
  | Cons (a, List a)

map : forall a. forall b. (a -> b) -> List a -> List b
map <a> <b> f xs =
  case xs of
    Nil _ -> []<b>
    Cons p ->
      Cons<b> (f (#2.1 p), map <a> <b> f (#2.2 p))

append : forall a. List a -> List a -> List a
append <a> l1 l2 =
  case l1 of
    Nil _ ->
      l2
    Cons p ->
      Cons<a> (#2.1 p, append <a> (#2.2 p) l2)

foldr : forall a. forall b. (a -> b -> b) -> b -> List a -> b
foldr <a> <b> f acc xs =
  case xs of
    Nil _ ->
      acc

    Cons p ->
      f (#2.1 p) (foldr <a> <b> f acc (#2.2 p))

concat : forall a. List (List a) -> List a
concat <a> = foldr <List a> <List a> (append <a>) []<a>

concatMap : forall a. forall b. (a -> List b) -> List a -> List b
concatMap <a> <b> f xs =
  case xs of
    Nil _ -> []<b>
    Cons p ->
      append<b> (f (#2.1 p)) (concatMap <a> <b> f (#2.2 p))

foldl : forall a. forall b. (a -> b -> a) -> a -> List b -> a
foldl <a> <b> f acc xs =
  case xs of
    Nil _ ->
      acc

    Cons p ->
      foldl <a> <b> f (f acc (#2.1 p)) (#2.2 p)

reverse : forall a. List a -> List a
reverse <a> = foldl <List a> <a> (\xs x -> Cons<a> (x, xs)) []<a>

type Maybe a
  = Nothing ()
  | Just a

type Nat
  = Z ()
  | S Nat

take : forall a. Nat -> List a -> List a
take <a> n xs =
  case n of
    Z _ -> []<a>
    S m ->
      case xs of
        Nil _ -> xs
        Cons p ->
          Cons<a> (#2.1 p, take <a> m (#2.2 p))

drop : forall a. Nat -> List a -> List a
drop <a> n xs =
  case n of
    Z _ -> xs
    S m ->
      case xs of
        Nil _ -> xs
        Cons p ->
          drop <a> m (#2.2 p)

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
