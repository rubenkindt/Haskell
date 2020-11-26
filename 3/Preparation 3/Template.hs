module Template where

data MyBool = MyTrue
            | MyFalse

data Exp = Const MyBool
         | And Exp Exp
         | Or Exp Exp

-- -------------------------------------------------
-- Equality Checking
-- -------------------------------------------------

instance Eq MyBool where
  (==) = error "Not implemented"

instance Eq Exp where
  (==) = error "Not implemented"

-- -------------------------------------------------
-- Printing
-- -------------------------------------------------

instance Show MyBool where
  show = error "Not implemented"

instance Show Exp where
  show = error "Not implemented"

-- -------------------------------------------------
-- Evaluating
-- -------------------------------------------------

class Evaluatable a where
  eval :: a -> Bool

instance Evaluatable MyBool where
  eval = error "Not implemented"

instance Evaluatable Exp where
  eval = error "Not implemented"
