module Myhaskell where


-------------------------------------------------------------------------------
-- PART I: Expense & Delta

-- IMPORTANT: MAKE SURE THE AMOUNT COMES BEFORE THE NAME
data Expense = EXPENSE_NOT_IMPLEMENTED deriving (Eq,Ord)

mkExpense :: String -> Double -> Expense
mkExpense = error "mkExpense: not yet implemented."

instance Show Expense where
  show _ = error "show: not yet implemented."

data Delta = DELTA_NOT_YET_IMPLEMENTED deriving (Eq,Ord)

instance Show Delta where
  show _ = error "show: not yet implemented."

fromExpense :: Double -> Expense -> Delta
fromExpense = error "fromExpense: not yet implemented."

mkDelta :: String -> Double -> Delta
mkDelta name amount = fromExpense 0 (mkExpense name amount)

-- | Convert a list of Expenses to a list of Deltas
-- The deltas are with respect to the average expense.
toDeltas :: [Expense] -> [Delta]
toDeltas = error "toDeltas: not yet implemented."

-------------------------------------------------------------------------------
-- PART II: Transferable Transfers

-- | The Transfer datatype: a money transfer from one person to another.
data Transfer = MkTransfer String String Double deriving Eq

trans_from :: Transfer -> String
trans_from (MkTransfer from _ _) = from

trans_to :: Transfer -> String
trans_to (MkTransfer _ to _) = to

trans_amount :: Transfer -> Double
trans_amount (MkTransfer _ _ amount) = amount

instance Show Transfer where
  show (MkTransfer from to amount) = from ++ " -> " ++ to ++ ":" ++ show amount

-- | The Transferable class contains types t to which a transfer can be applied,
-- and that can create a Transfer from one t to another t
class Transferable t where
  applyTransfer  :: Transfer -> t -> t

-- | Apply a list of Transfers to to a Transferable from left to right.
applyTransfers :: Transferable t => [Transfer] -> t -> t
applyTransfers transfers x = foldl (flip applyTransfer) x transfers

instance Transferable Expense where
  -- IMPLEMENTATION REQUIRED

instance Transferable Delta where
  -- IMPLEMENTATION REQUIRED

createTransfer :: Double -> Delta -> Delta -> Transfer
createTransfer = error "createTransfer: not yet implemented."

-------------------------------------------------------------------------------
-- PART III: Balancing Expenses

-- | Check if a list of expenses is epsilon-balanced.
balanced :: [Expense] -> Double -> Bool
balanced = error "balanced: not yet implemented."

-- | Epsilon-balance a list of Deltas.
balanceDeltas :: [Delta] -> Double -> [Transfer]
balanceDeltas = error "balanceDeltas: not yet implemented."

-- | Epsilon-balance a list of Expenses.
balance :: [Expense] -> Double -> [Transfer]
balance = error "balance: not yet implemented."


-------------------------------------------------------------------------------
-- PART IV: Application

-- | Read a list of expenses.
-- If the entered amount is non-negative, the list is terminated.
getExpenses :: IO [Expense]
getExpenses = error "getExpenses: not yet implemented."

-- | Print a list of transfers, each on a separate line 
printTransfers :: [Transfer] -> IO ()
printTransfers = error "printTransfers: not yet implemented."

-- | Read a list of Expenses, balance them, and print the required transfers.
balanceIO :: IO ()
balanceIO = error "balanceIO: not yet implemented."

{- Example:

> balanceIO
Name: Alex
Amount: 200
Name: Tom
Amount: 1000
Name: Thomas
Amount: 275.5
Name: 
Amount: 0
Gert-Jan -> Tom:338.875
Alex -> Tom:178.875
Thomas -> Tom:103.375

-}
