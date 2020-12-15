module MyHaskell where

import Data.List
-- Task 1a

data Circuit
  = Cir String 
  | Not Circuit
  | And Circuit Circuit
  | Or  Circuit Circuit
  | Xor Circuit Circuit

-- Task 1b

cinput :: String -> Circuit 
cinput = Cir

cnot   :: Circuit -> Circuit
cnot  c  = Not c 

cand   :: Circuit -> Circuit -> Circuit
cand c1 c2  = And c1 c2

cor    :: Circuit -> Circuit -> Circuit
cor c1 c2   = Or c1 c2

cxor   :: Circuit -> Circuit -> Circuit
cxor c1 c2  = Xor c1 c2

-- Task 1c

example :: Circuit
example = cor (cand (cinput "x") (cinput "y")) (cxor (cnot (cinput "z")) (cinput "x"))

-- Task 1d

candMany :: [Circuit] -> Circuit
candMany (c:[]) = cand (c) (c)
candMany (c:cs) = cand (c) (candMany cs)

-- Task 2a

instance Show Circuit where
  show (Cir str)   = str
  show (Not c1)    = "NOT(" ++ show c1 ++ ")"
  show (And c1 c2) = "AND(" ++ show c1 ++ "," ++ show c2 ++ ")"
  show (Or  c1 c2) = "OR("  ++ show c1 ++ "," ++ show c2 ++ ")"
  show (Xor c1 c2) = "XOR(" ++ show c1 ++ "," ++ show c2 ++ ")"
  

-- Task 2b

simplify :: Circuit -> Circuit
simplify c = simplifyOr $ simplifyExor c

simplifyExor :: Circuit -> Circuit
simplifyExor (Cir s)   = Cir s
simplifyExor (Not x )  = cnot (simplifyExor x)
simplifyExor (And x y) = cand (simplifyExor x) (simplifyExor y)
simplifyExor (Or x y) = cnot (cand (cnot (simplifyExor x)) (cnot (simplifyExor y))) 
simplifyExor (Xor x1 y1) = cor (cand (x) (cnot y)) (cand (cnot x) (y)) where
 x=simplifyExor x1
 y=simplifyExor y1
 

simplifyOr :: Circuit -> Circuit
simplifyOr (Cir s)   = Cir s
simplifyOr (Not x )  = cnot (simplifyOr x)
simplifyOr (And x y) = cand (simplifyOr x) (simplifyOr y)
simplifyOr (Or x1 y1) = cnot (cand (cnot (x)) (cnot (y)) ) where
 x=simplifyOr x1
 y=simplifyOr y1
 


-- Task 2c

size :: Circuit -> Int
size (Cir s)    = 0
size (Not x )   = 1 +  size  x
size (And x y)  = 1 + (size x )  + (size y)
size (Or x y)   = 1 + (size x )  + (size y)
size (Xor x y)  = 1 + (size x )  + (size y)

-- Task 2d

gateDelay :: Circuit -> Int
gateDelay (Cir s)    = 0
gateDelay (Not x )   = 1 +  gateDelay x
gateDelay (And x y)  = 1 + maximum ([gateDelay x] ++ [gateDelay y])
gateDelay (Or x y)   = 1 + maximum ([gateDelay x] ++ [gateDelay y])
gateDelay (Xor x y)  = 1 + maximum ([gateDelay x] ++ [gateDelay y])

-- Task 2e

inputs :: Circuit -> [String]
inputs = nub . getInputs

getInputs :: Circuit -> [String]
getInputs (Cir s)    = [s]
getInputs (Not x )   = getInputs x
getInputs (And x y)  = (getInputs x) ++ (getInputs y)
getInputs (Or x y)   = (getInputs x) ++ (getInputs y)
getInputs (Xor x y)  = (getInputs x) ++ (getInputs y) 

-- Task 3a

simulate :: Circuit -> [(String,Bool)] -> Bool
simulate (Cir s)   list =  str2bool s list
simulate (Not x )  list = not (simulate x list)
simulate (And x y) list = (simulate x list) &&   (simulate y list)
simulate (Or x y)  list = (simulate x list) ||   (simulate y list)
simulate (Xor x y) list = (simulate x list) `xor`(simulate y list)

str2bool :: String -> [(String,Bool)] -> Bool
str2bool _ [] = error "not in list"
str2bool str ((s,b):xs) = if s==str then b else str2bool str xs

xor :: Bool -> Bool -> Bool
xor False True =True
xor True False =True
xor _  _ =False

-- Task 3b

combinations :: Int -> [[Bool]]
combinations 0 = [[]]
combinations times = (map (False:) list ) ++ (map (True:) list)
 where
  list = combinations (times-1)

-- Task 3c
tabulate :: Circuit -> IO ()
tabulate = putStrLn . cir2String


cir2String :: Circuit -> String
cir2String c = myHeader c ++ myBody c


myHeader :: Circuit -> String 
myHeader c = unwords $ ((inputs c) ++ [" | output \n"])

myBody :: Circuit -> String 
myBody c = unlines [ unwords ((inputString list) ++ [" | "] ++ [b2str result])  | (result,list) <- simulated]
 where 
  len = length $ inputs c
  combis  = combinations len
  simulated = [ ((simulate c inpComb),inpComb) | comb <- (combinations len), let inpComb = (zip (inputs c) comb)]
  
inputString :: [(String,Bool)] -> [String]
inputString [] = []
inputString ((str,b):xs) = [b2str b] ++ (inputString xs)
 
b2str :: Bool -> String
b2str True  = "1" 
b2str False = "0" 

  

-- -- Task 4a
-- 
-- check :: Circuit -> [(String,Bool)] -> Bool -> Bool
-- check c env r = error "undefined"
-- 
-- checkAll :: [([(String,Bool)],Bool)] -> Circuit -> Bool
-- checkAll = error "undefined"
-- 
-- -- Task 4b
-- 
-- splits :: Int -> [(Int,Int)]
-- splits = error "undefined"
-- 
-- -- Task 4c
-- 
-- generate :: [String] -> Int -> [Circuit]
-- generate = error "undefined"
-- 
-- -- Task 4d
-- 
-- smallest :: [String] -> [([(String,Bool)],Bool)] -> Circuit
-- smallest = error "undefined"
  
