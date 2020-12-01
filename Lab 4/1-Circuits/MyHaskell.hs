module MyHaskell where

-- Task 1a

data Circuit
  = DEFINE_ME

-- Task 1b

cinput :: String -> Circuit 
cinput = undefined

cnot   :: Circuit -> Circuit
cnot   = undefined

cand   :: Circuit -> Circuit -> Circuit
cand   = undefined

cor    :: Circuit -> Circuit -> Circuit
cor    = undefined

cxor   :: Circuit -> Circuit -> Circuit
cxor   = undefined

-- Task 1c

example :: Circuit
example = undefined

-- Task 1d

candMany :: [Circuit] -> Circuit
candMany (c:cs) = undefined

-- Task 2a

instance Show Circuit where
  show = undefined

-- Task 2b

simplify :: Circuit -> Circuit
simplify = undefined

-- Task 2c

size :: Circuit -> Int
size = undefined

-- Task 2d

gateDelay :: Circuit -> Int
gateDelay = undefined

-- Task 2e

inputs :: Circuit -> [String]
inputs = undefined

-- Task 3a

simulate :: Circuit -> [(String,Bool)] -> Bool
simulate = undefined

-- Task 3b

combinations :: Int -> [[Bool]]
combinations = undefined

-- Task 3c
tabulate :: Circuit -> IO ()
tabulate = undefined


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
  
