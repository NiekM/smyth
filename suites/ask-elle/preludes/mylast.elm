type List a
  = Nil ()
  | Cons (a, List a)

type Maybe a
  = Nothing ()
  | Just a

-- TODO: partial function

-- TODO: add safe_head && reverse
-- TODO: add foldr1, flip && const
-- TODO: add index, length && subtract

--foldr : forall a. forall b. (a -> b -> b) -> b -> List a -> b
--foldr <a> <b> f acc xs =
--  case xs of
--    Nil _ ->
--      acc
--
--    Cons p ->
--      f (#2.1 p) (foldr <a> <b> f acc (#2.2 p))
