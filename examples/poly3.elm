type Nat
  = Z ()
  | S Nat

type List a
  = Nil ()
  | Cons (a, List<a>)

listStutter : forall a . List<a> -> List<a>
listStutter <a> xs =
  ??

specifyFunction (listStutter <Nat>)
  [ (Nil<Nat> (), Nil<Nat> ())
  -- , ([0], [0, 0])
  -- , ([1, 0], [1, 1, 0, 0])
  ]