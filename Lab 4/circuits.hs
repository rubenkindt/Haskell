module MyHaskell where

-- Task 1a

data Circuit
  = NAME String | NOT Circuit | AND [Circuit] | OR [Circuit] | XOR Circuit Circuit

--data Input = String Bool deriving (Eq, Show)
-- Task 1b

cinput :: String -> Circuit 
cinput str = (NAME str)

cnot   :: Circuit -> Circuit
cnot  c =  NOT c

cand   :: Circuit -> Circuit -> Circuit
cand  c1 c2 =  (AND (c1:c2:[]))

cor    :: Circuit -> Circuit -> Circuit
cor   c1 c2 =  (OR (c1:c2:[]))

cxor   :: Circuit -> Circuit -> Circuit
cxor  c1 c2 = (XOR c1 c2)

-- Task 1c
example :: Circuit
example = (OR [(AND [(NAME "x"),(NAME "y")]),(XOR (NOT (NAME "z")) (NAME "x"))])

example2 :: Circuit
example2 = (XOR (NAME "x") (NAME "z"))

example3 :: Circuit
example3 = (OR [(NAME "x"),(NAME "z")])

-- Task 1d

candMany :: [Circuit] -> Circuit
candMany ((NAME c):[]) = (NAME c)
candMany ((NOT c):[])  = (NOT c)
candMany (c:cs) = (AND (c:cs))

-- Task 2a
instance Show Circuit where
  show (NAME str)    = str
  show (NOT c)       = "NOT(" ++ show c   ++ ")"
  show (AND (c:cs))  = "AND(" ++ foldr (\x xs-> if xs == "" then show x  else show x ++ "," ++ xs) "" (c:cs) ++ ")"
  show (OR  (c:cs))  = "OR("  ++ foldr (\x xs-> if xs == "" then show x  else show x ++ "," ++ xs) "" (c:cs) ++ ")"
  show (XOR c1 c2)   = "XOR(" ++ show c1  ++ "," ++ show c2 ++ ")"
  
--  show (AND (c:cs))  = "AND(" ++ show (showList (c:cs)) ++ ")"
--  show (OR  (c:cs))  = "OR("  ++ show (showList (c:cs)) ++ ")"
--  OR(AND("x","\"y\",\"\""),"XOR(\"x\",NOT(\"z\")),\"\"")
{-
instance (Show Circuit) => Show [Circuit] where
  show (x:[])        = show x
  show (x:xs)        = show x ++ "," ++ show xs
-}

-- Task 2b

simplify :: Circuit -> Circuit
simplify = simplifyOR . simplifyXOR

simplifyXOR :: Circuit -> Circuit
simplifyXOR (NAME str) = (NAME str)
simplifyXOR (NOT c)    = (NOT (simplifyXOR c))
simplifyXOR (AND list) = (AND [simplifyXOR x |x <- list])
simplifyXOR (OR list)  = (OR [simplifyXOR x |x <- list])
simplifyXOR (XOR x1 y1)= (OR [(AND [x,(NOT y)]),(AND [(NOT x),y])])
  where x=(simplifyXOR x1)
        y=(simplifyXOR y1)

--  = NAME String | NOT Circuit | AND [Circuit] | OR [Circuit] | XOR Circuit Circuit


simplifyOR :: Circuit -> Circuit
simplifyOR (NAME str) = (NAME str)
simplifyOR (NOT c)    = (NOT (simplifyOR c))
simplifyOR (AND list) = (AND [simplifyOR x |x <- list])
simplifyOR (OR  list) = (NOT (AND [(NOT (simplifyOR x))|x<-list ]))
simplifyOR (XOR x1 y1)= error "xor's should not exist here" 

--NOT(AND(NOT(AND(x,y)),NOT(NOT(AND(NOT(AND(NOT(z),NOT(x))),NOT(AND(NOT(NOT(z)),x)))))))
--NOT(AND(NOT(AND(x,y)),NOT(NOT(AND(NOT(AND(NOT(z),NOT(x))),NOT(AND(NOT(NOT(z)),x)))))))

-- Task 2c

size :: Circuit -> Int
size (NAME str) = 0
size (NOT c)    = 1 + (size c)
size (AND list) = 1 + sum [size x |x <- list]
size (OR  list) = 1 + sum [size x |x <- list]
size (XOR x1 y1)= 1 + size x1 + size y1

-- Task 2d

gateDelay :: Circuit -> Int
gateDelay (NAME str) = 0
gateDelay (NOT c)    = 1 + (gateDelay c)
gateDelay (AND list) = 1 + maximum [gateDelay x |x <- list]
gateDelay (OR  list) = 1 + maximum [gateDelay x |x <- list]
gateDelay (XOR x1 y1)= 1 + maximum [(gateDelay x1),(gateDelay y1)]

-- Task 2e

inputs :: Circuit -> [String]
inputs c = remDups (getInputs c) []

getInputs :: Circuit -> [String]
getInputs (NAME str) = [str]
getInputs (NOT c)    = getInputs c
getInputs (AND list) = concat [getInputs x |x <- list]
getInputs (OR  list) = concat [getInputs x |x <- list]
getInputs (XOR x1 y1)= (getInputs x1) ++ (getInputs y1)

--stolen from https://self-learning-java-tutorial.blogspot.com/2016/06/haskell-remove-duplicate-elements-from.html
remDups :: (Eq a) => [a] -> [a] -> [a]
remDups [] _ = []
remDups (x:xs) list2
    | (x `elem` list2) = remDups xs list2
    | otherwise = x : remDups xs (x:list2)

-- Task 3a

simulate :: Circuit -> [(String,Bool)] -> Bool
simulate (NAME str)  ((s,bool):xs) = if str==s then bool else simulate (NAME str) xs
simulate (NOT c)     boolList = not (simulate c boolList)
simulate (AND list)  boolList = and [simulate x boolList | x <- list]
simulate (OR  list)  boolList = or  [simulate x boolList | x <- list]
simulate (XOR x1 y1) boolList = xor (simulate x1 boolList) (simulate y1 boolList)

xor :: Bool -> Bool -> Bool
xor x y = if x/=y then True else False

-- Task 3b

combinations :: Int -> [[Bool]]
combinations 0 = [[]]
combinations 1 = [[False],[True]]
combinations 2 = [[False,False],[False,True],[True,False],[True,True]]
combinations 3 = [[False,False,False],[False,False,True],[False,True,False],[False,True,True],[True,False,False],[True,False,True],[True,True,False],[True,True,True]]




-- Task 3c

tabulate :: Circuit -> IO ()
tabulate c = myPrint c ([  (merge (inputs c) com) ++ [("O", (simulate c (merge (inputs c) com)) )]  | com <- combinations size  ])
  where 
    size =length $ inputs c

-- dit is de ZIP functie
merge :: [a] -> [b] -> [(a,b)]
merge (x:xs) (y:ys) = [(x, y)] ++ (merge xs ys) -- cleaner (x,y) : (merge xs ys)
--merge _ _ = []

-- 



myPrint :: Circuit -> [[(a,Bool)]] -> IO ()
myPrint c list = myHeader c >> myBody list
--myPrint _ _ = return ()


myHeader :: Circuit -> IO ()
myHeader c = (putStr . concat . map (++ " ") $ getInputs c ) >> putStrLn "| Output" 
--myHeader _ = return ()

myBody :: [[(a,Bool)]] -> IO ()
myBody (oneLineList:xs) = oneLine oneLineList >> myBody xs
--myBody _ = return ()

oneLine :: [(a,Bool)] -> IO ()
oneLine ((letter,bool):[])= putStr "| " >> putStrLn (boolToInt bool)
oneLine ((letter,bool):xs)= putStr (boolToInt bool) >> putStr " " >> oneLine xs
oneLine [] = return ()
--oneLine _ = return ()

boolToInt :: Bool -> String
boolToInt True  = "1"
boolToInt False = "0"
--boolToInt _ = "error"

-- -- Task 4a
-- 
-- check :: Circuit -> [(String,Bool)] -> Bool -> Bool
-- check c env r = undefined
-- 
-- checkAll :: [([(String,Bool)],Bool)] -> Circuit -> Bool
-- checkAll = undefined
-- 
-- -- Task 4b
-- 
-- splits :: Int -> [(Int,Int)]
-- splits = undefined
-- 
-- -- Task 4c
-- 
-- generate :: [String] -> Int -> [Circuit]
-- generate = undefined
-- 
-- -- Task 4d
-- 
-- smallest :: [String] -> [([(String,Bool)],Bool)] -> Circuit
-- smallest = undefined
  
