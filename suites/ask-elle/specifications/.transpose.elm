-- TODO: actually update these i.o. being equal to examples
specifyFunction (transpose <Nat>)
  [ ([], [])
  , ([[]], [[]])
  , ([[0],[1]], [[0, 1]])
  , ([[0, 1, 2]], [[0], [1], [2]])
  , ([[0, 1], [1, 0]], [[0, 1], [0, 1]])
  , ([[0, 1], [0, 1]], [[0, 0], [1, 1]])
  ]
