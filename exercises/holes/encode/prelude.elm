type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

type Encoding
  = None ()
  | Encode (Nat, Nat, Encoding)
 