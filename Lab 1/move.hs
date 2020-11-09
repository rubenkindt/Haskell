data Move = Roke | Paper | Scissors
data Result = Win | Lose | Draw

beat :: Move -> Move
beat Roke =Paper
beat Paper =Scissors
beat Scissors =Roke

lose :: Move -> Move
lose Roke =Scissors
lose Paper =Roke
lose Scissors =Paper

outcome :: Move -> Move -> Result
outcome Roke Roke         =Draw
outcome Scissors Scissors =Draw
outcome Paper Paper       =Draw
outcome Roke Scissors     =Win
outcome Scissors Paper    =Win
outcome Paper Roke        =Win
outcome Roke Paper        =Lose
outcome Scissors Roke     =Lose
outcome Paper Scissors    =Lose


--------------------------
equal :: Move -> Move -> Bool
equal Roke Roke         = True
equal Scissors Scissors = True
equal Paper Paper       = True

outcome2 :: Move -> Move -> Result
outcome2 p1 p2
 | equal (lose p1) p2 = Lose
 | equal (beat p1) p2 = Win
 | otherwise =Draw
 
