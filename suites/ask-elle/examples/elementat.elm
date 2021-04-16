specifyFunction2 (elementat <Nat>)
  [ ([], 0, Nothing)
  , ([], 1, Nothing)
  , ([0], 0, Just 0)
  , ([1, 0], 2, Nothing)
  , ([1, 3, 2], 1, Just 3)
  , ([1, 3, 2], 2, Just 2)
  , ([1, 3, 2, 0], 3, Just 0)
  ]
