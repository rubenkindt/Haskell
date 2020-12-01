
module Template where

import Data.Char

-- * Sequences
-- ----------------------------------------------------------------------------

class Sequence a where
  prev :: a -> a
  next :: a -> a

instance Sequence Int where
  prev i = (-) i 1
  next i = (+) i 1

instance Sequence Char where
  prev letter = chr (ord letter - 1)
  next letter = chr (ord letter + 1)

instance Sequence Bool where
  prev = not 
  next = not


class Sequence a => LeftBoundedSequence a where
  firstElem :: a
  
instance LeftBoundedSequence Char where
  firstElem = 'a'

instance LeftBoundedSequence Bool where
  firstElem = False
  
  
class Sequence a => RightBoundedSequence a where
  lastElem :: a

instance RightBoundedSequence Char where
  lastElem = 'z'

instance RightBoundedSequence Bool where
  lastElem = True

