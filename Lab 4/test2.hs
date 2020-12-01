
data Circuit
  = NAME String | NOT Circuit | AND [Circuit] | OR [Circuit] | XOR Circuit Circuit deriving (Eq, Show)

--data Input = String Bool deriving (Eq, Show)
-- Task 1b

cinput :: String -> Circuit 
cinput str = (NAME str)

cnot   :: Circuit -> Circuit
cnot  c =  (NOT c)
cnot  c =  (NOT c)

cand   :: Circuit -> Circuit -> Circuit
cand  c1 c2 =  (AND (c1:c2))