specifyFunction2 (split <Nat>)
  [ ([], 0, ([], []))
  , ([], 1, ([], []))
  , ([0], 0, ([], [0]))
  , ([0], 1, ([0], []))
  , ([1, 0], 2, ([1, 0], []))
  , ([1, 3, 2], 1, ([1], [3, 2]))
  , ([1, 3, 2], 2, ([1, 3], [2]))
  , ([1, 3, 2, 0], 3, ([1, 3, 2], [0]))
  ]
