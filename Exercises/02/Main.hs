import qualified System.Random as R

binary_random_response :: R.StdGen -> Bool -> (Bool, R.StdGen)
binary_random_response g true_answer =
  if first_coin then (true_answer, g')
  else R.random g'
  where (first_coin, g') = R.random g