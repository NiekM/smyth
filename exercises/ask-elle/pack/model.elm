helper : Nat -> List (List Nat) -> List Nat
helper x xss =
  case xss of
    LNil _ -> LCons ([x], LNil ())
    LCons p ->
      -- TODO: add compare or smth and head function etc.

pack : NatList -> NatListList
pack = foldr <NatListList> helper (LNil ())
