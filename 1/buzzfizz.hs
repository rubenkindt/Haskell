buzzfizz :: Int -> String
buzzfizz x 
  |x `rem` 15 ==0 ="Buzzfizz"
  |x `rem` 3 ==0 ="fizz"
  |x `rem` 5 ==0 ="buzz"
  |otherwise =show x
