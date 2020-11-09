data Exp = Const Int
 | Add Exp Exp
 | Sub Exp Exp
 | Mul Exp Exp 
 deriving (Show, Eq)
 
eval :: Exp -> Int
eval (Const exp) =exp
eval (Add exp1 exp2) = eval exp1 + eval exp2
eval (Sub exp1 exp2) = eval exp1 - eval exp2
eval (Mul exp1 exp2) = eval exp1 * eval exp2

