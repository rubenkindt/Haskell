module Template where

data Tree a = Leaf a | Fork (Tree a) (Tree a)
  deriving (Show, Eq)

foldTree :: (a -> b) -> (b -> b -> b) -> (Tree a -> b)
foldTree = error "Not implemented!"

sumTree :: Tree Int -> Int
sumTree = error "Not implemented!"

treeToList :: Tree a -> [a]
treeToList = error "Not implemented!"

nrOfLeaves :: Tree a -> Int
nrOfLeaves = error "Not implemented!"

depthOfTree :: Tree a -> Int
depthOfTree = error "Not implemented!"

mirrorTree :: Tree a -> Tree a
mirrorTree = error "Not implemented!"

minTree :: Tree Int -> Int
minTree = error "Not implemented!"

addOne :: Tree Int -> Tree Int
addOne = error "Not implemented!"

idTree :: Tree a -> Tree a
idTree = error "Not implemented!"
