-- TODO: fix this, add compare function etc.
encode : NatList -> Encoding
encode xs =
  case xs of
    Nil _ -> None ()
    Cons p -> ??
