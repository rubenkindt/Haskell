data Circuit
  = Circuit String | AND Circuit Circuit
  
--data Input = String Bool deriving (Eq, Show)
-- Task 1b

cinput :: String -> Circuit 
cinput str = Circuit str

cand   :: Circuit -> Circuit -> Circuit
cand  c1 c2 = (AND c1 c2)