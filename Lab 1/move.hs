data Move = Roke | Paper | Scissors deriving Eq
data Result = Win | Lose | Draw deriving Show

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

{-
outcome Roke Paper        =Lose
outcome Scissors Roke     =Lose
outcome Paper Scissors    =Lose

overbodig 
als 
outcome _ _ = Lose
_ is gevaarlijk, moet onderaan staan, 
				warnings 'not all cases are are used' krijg je dan niet
-}


--------------------------
equal :: Move -> Move -> Bool
equal Roke Roke         = True
equal Scissors Scissors = True
equal Paper Paper       = True

outcome2 :: Move -> Move -> Result
outcome2 p1 p2
 | (lose p1) == p2 = Lose
 | (beat p1) == p2 = Win
 | otherwise =Draw
 
