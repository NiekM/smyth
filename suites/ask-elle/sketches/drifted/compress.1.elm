dropWhile : (Nat -> Bool) -> List Nat -> List Nat
dropWhile f xs =
  case xs of
    Nil _ -> []<Nat>
    Cons p ->
      case f (#2.1 p) of
        F _ -> dropWhile f (#2.2 p)
        T _ -> xs


compress : List Nat -> List Nat
compress xs = foldr <Nat -> Bool> <List Nat> (\x -> dropWhile x) xs ??
