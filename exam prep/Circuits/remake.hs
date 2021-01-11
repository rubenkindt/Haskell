module MyHaskell where

import Data.List
-- Task 1a

data Circuit
  = C String
  | NOT Circuit
  | AND Circuit Circuit
  | OR  Circuit Circuit
  | XOR Circuit Circuit
  
-- Task 1b

cinput :: String -> Circuit 
cinput = C

cnot   :: Circuit -> Circuit
cnot   = NOT 

cand   :: Circuit -> Circuit -> Circuit
cand   = AND

cor    :: Circuit -> Circuit -> Circuit
cor    = OR

cxor   :: Circuit -> Circuit -> Circuit
cxor   = XOR

-- Task 1c

example :: Circuit
example = cor (cand (C "x") (C "y")) (cxor (cnot (C "z")) (C "x"))

-- Task 1d

candMany :: [Circuit] -> Circuit
candMany (c:[]) = c
candMany (c:cs) = cand c (candMany cs)

-- Task 2a

instance Show Circuit where
  show (C string) = string
  show (NOT c   ) = "NOT(" ++ show c ++ ")"
  show (AND c c2) = "AND(" ++ show c ++ "," ++ show c2 ++ ")"
  show (OR  c c2) = "OR("  ++ show c ++ "," ++ show c2 ++ ")"
  show (XOR c c2) = "XOR(" ++ show c ++ "," ++ show c2 ++ ")"
  

-- Task 2b

simplify :: Circuit -> Circuit
simplify = simplifyOr . simplifyXor

simplifyXor :: Circuit -> Circuit
simplifyXor (C string) = (C string)
simplifyXor (NOT c   ) = (NOT c1   ) where
  c1 = simplifyXor c
simplifyXor (AND c c2) = (AND c1 c3) where
 c1 = simplifyXor c
 c3 = simplifyXor c2
simplifyXor (OR  c c2) = (OR  c1 c3) where
 c1 = simplifyXor c
 c3 = simplifyXor c2
simplifyXor (XOR c c2) = cor (cand c1 (cnot(c3)) ) (cand (cnot(c1)) c3) where
 c1 = simplifyXor c
 c3 = simplifyXor c2
 
simplifyOr :: Circuit -> Circuit
simplifyOr (C string) = (C string)
simplifyOr (NOT c   ) = (NOT c1   ) where
  c1 = simplifyOr c
simplifyOr (AND c c2) = (AND c1 c3) where
 c1 = simplifyOr c
 c3 = simplifyOr c2
simplifyOr (OR  c c2) = cnot(cand (cnot(c1)) (cnot(c3)) ) where
 c1 = simplifyOr c
 c3 = simplifyOr c2

-- Task 2c

size :: Circuit -> Int
size (C string) = 0
size (NOT c   ) = 1 + (size c)
size (AND c c2) = 1 + (size c) + (size c2)
size (OR  c c2) = 1 + (size c) + (size c2)
size (XOR c c2) = 1 + (size c) + (size c2)

-- Task 2d

gateDelay :: Circuit -> Int
gateDelay (C string) = 0
gateDelay (NOT c   ) = 1 + (gateDelay c)
gateDelay (AND c c2) = 1 + max(gateDelay c) (gateDelay c2)
gateDelay (OR  c c2) = 1 + max(gateDelay c) (gateDelay c2)
gateDelay (XOR c c2) = 1 + max(gateDelay c) (gateDelay c2)

-- Task 2e

inputs :: Circuit -> [String]
inputs (C string) = [string]
inputs (NOT c   ) = inputs c
inputs (AND c c2) = nub ((inputs c) ++ (inputs c2))
inputs (OR  c c2) = nub ((inputs c) ++ (inputs c2))
inputs (XOR c c2) = nub ((inputs c) ++ (inputs c2))


-- Task 3a

simulate :: Circuit -> [(String,Bool)] -> Bool
simulate (C string) ((x,b):xs) = if string==x then b else simulate (C string) xs
simulate (NOT c   ) l = not(simulate c l)
simulate (AND c c2) l = (simulate c l) && (simulate c2 l)
simulate (OR  c c2) l = (simulate c l) || (simulate c2 l)
simulate (XOR c c2) l = (simulate c l) `xor` (simulate c2 l)

xor::Bool->Bool->Bool
xor = (/=)

-- Task 3b

combinations :: Int -> [[Bool]]
combinations 0 = [[]]
combinations n = (map ([False] ++) prev) ++ (map ([True] ++) prev)
 where 
  prev = combinations (n-1)


-- Task 3c
tabulate :: Circuit -> IO ()
tabulate c = (putStrLn $ tabulate_header c) >>
 mapM_ (putStrLn . tabulate_body c) (combinations len)
  where 
   len = length(inputs c)

tabulate_header :: Circuit -> String
tabulate_header c = ((concat . map (++ " ") $ inputs c) ++ "| " ++ "output")

tabulate_body :: Circuit -> [Bool] -> String
tabulate_body c b = (b2lmap b) ++ "| " ++ (b2l (simulate c (zip (inputs c) b)))
 where 
  b2lmap = concat . map b2l
  b2l True="1 "
  b2l False="0 "



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
  
