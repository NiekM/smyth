type Nat
  = Z ()
  | S Nat

type List a
  = Nil ()
  | Cons (a, List a)

foldr : forall a . (a -> List a -> List a) -> List a -> List a -> List a
foldr <a> f acc xs =
  case xs of
    Nil _ ->
      acc

    Cons p ->
      f (#2.1 p) (foldr <a> f acc (#2.2 p))

cons : forall a . a -> List a -> List a
cons <a> x xs = Cons<a> (x, xs)

id : forall a . List a -> List a
id <a> = ?? 

specifyFunction (id<Nat>)
  [ ([]<Nat>, []<Nat>)
  , ([1]<Nat>, [1]<Nat>)
  , ([4]<Nat>, [4]<Nat>)
  , ([3,2]<Nat>, [3,2]<Nat>)
  , ([1,2,3]<Nat>, [1,2,3]<Nat>)
  ]
