data Inst = IPush Int 
 | IAdd 
 | ISub 
 | IMul
 deriving (Show, Eq)

type Prog   = [Inst]
type Stack  = [Int]

data Exp = Const Int
 | Add Exp Exp
 | Sub Exp Exp
 | Mul Exp Exp 
 deriving (Show, Eq)

add :: Int -> Int -> Int
add int1 int2 = int1 + int2

sub :: Int -> Int -> Int
sub int1 int2 = int1 - int2

mul :: Int -> Int -> Int
mul int1 int2 = int1 * int2

execute :: Inst -> Stack -> Stack
execute operation [] =[]
execute operation (iets:[]) = runtimeError
execute (IPush nr) lijst      = (nr:lijst)
execute IAdd (int1:int2:tail) = ((add int2 int1)  :tail)
execute ISub (int1:int2:tail) = ((sub int2 int1)  :tail)
execute IMul (int1:int2:tail) = ((mul int2 int1)  :tail)

run :: Prog -> Stack -> Stack
run [] lijst = lijst
run [ope] [] = runtimeError
run ((IPush nr):tailOperation) lijst      = run tailOperation (nr:lijst)
run (IAdd:tailOperation) (int1:int2:tail) = run tailOperation ((add int2 int1):tail)
run (ISub:tailOperation) (int1:int2:tail) = run tailOperation ((sub int2 int1):tail)
run (IMul:tailOperation) (int1:int2:tail) = run tailOperation ((mul int2 int1):tail)


--compile :: Exp -> Prog
--compile exp = ((reverce nrs) : operation)
-- where (nrs,operation) = comileHelper exp

reverce :: [Int] -> [Int]
reverce [] = []
reverce (head:tail) = (reverce tail :head)

--comileHelper :: Exp -> ([],[])
--comileHelper [] = ([],[])
--comileHelper (Sub:tail)= (nrs,(Sub:operations))


runtimeError :: Stack
runtimeError = error "Runtime error."


