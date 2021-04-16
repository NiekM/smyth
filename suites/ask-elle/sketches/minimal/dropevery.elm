-- NOTE: adding a helper function is enough to synthesize dropevery

dropevery : forall a. List a -> Nat -> List a
dropevery <a> xs n =
    -- NOTE: we can recognise a helper function by noting that
    --       a let binding is introduced at a non-arrow type
  let
    helper : List a -> Nat -> List a
    helper = ??
  in
    helper xs n
 