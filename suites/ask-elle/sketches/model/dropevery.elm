dropevery : forall a. List a -> Nat -> List a
dropevery <a> xs n =
  let
    helper : List a -> Nat -> List a
    helper ys m =
      case ys of
        Nil _ -> []<a>
        Cons p ->
          case m of
            Z _ -> helper (#2.2 p) n
            S k -> Cons<a> (#2.1 p, helper (#2.2 p) k)
  in
    helper xs n
 