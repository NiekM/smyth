type Nat
  = Z ()
  | S Nat

type List a
  = Nil ()
  | Cons (a, List a)

foldr : forall a. forall b. (a -> b -> b) -> b -> List a -> b
foldr <a> <b> f acc xs =
  case xs of
    Nil _ ->
      acc

    Cons p ->
      f (#2.1 p) (foldr <a> <b> f acc (#2.2 p))
 
cat : forall a. List a -> List a -> List a
cat <a> xs ys =
  case xs of
    Nil _ -> ys
    Cons p -> Cons <a> (#2.1 p, cat <a> (#2.2 p) ys)
