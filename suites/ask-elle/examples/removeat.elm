specifyFunction2 (removeat <Nat>)
  [ (0, [], (Nothing, []))
  , (1, [], (Nothing, []))
  , (0, [0], (Just 0, []))
  , (2, [1, 0], (Nothing, [1, 0]))
  , (1, [1, 3, 2], (Just 3, [1, 2]))
  , (2, [1, 3, 2], (Just 2, [1, 3]))
  , (3, [1, 3, 2, 0], (Just 0, [1, 3, 2]))
  ]
