
------------------------------------------------------
------------------------------------------------------
------------------------------------------------------
------------------------------------------------------
   (before,aft) = break (\(W str) -> (length str) > nr) list
   
------------------------------------------------------

--een guard kan ej gebruiken zonder de test al te definieren 
-- je scrijft de test bij de 
--"| test ="
wrap :: Int -> [LineItem] -> [[LineItem]]
wrap lineWidth = go [] (lineWidth + 1)
  -- One extra space to offset the space of the first word
  where
    go acc _          [] = [reverse acc]
    go acc spaceLeft (W w:is)
        | length w + 1 <= spaceLeft || length w > lineWidth
        = go (W w:acc) (spaceLeft - length w - 1) is
        | otherwise
        = reverse acc : go [] (lineWidth + 1) (W w:is)
		
------------------------------------------------------

infix 4 ~=
--4 indicates precedence level, 

------------------------------------------------------

(>->) :: Fractal -> Fractal -> Fractal
FDone     >-> f  =  f
FTurn a r >-> f  =  FTurn a (r >-> f)
FStep b r >-> f  =  FStep b (r >-> f)

------------------------------------------------------


--Truck geef True en False een kleur 
> combinations 0
[[]]
> combinations 1
[[False],[True]]
> combinations 2
[[False,False],[False,True],[True,False],[True,True]]
> combinations 3
[[False,False,False],[False,False,True],[False,True,False],[False,True,True]
 ,[True,False,False],[True,False,True],[True,True,False],[True,True,True]]

combinations :: Int -> [[Bool]]
combinations 0 = [[]]
combinations n = let cs = combinations (n-1)
                 in map (False:) cs ++ map (True:) cs

------------------------------------------------------

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort (x:xs) = small:selectionSort (delete small (x:xs)) 
 where small = smallest x xs
-- can probl be done with map, filter or foldr

smallest :: Ord a => a -> [a] -> a
smallest small [] = small
smallest small (x:xs) 
 | small > x = smallest x xs 
 | otherwise = smallest small xs
 
------------------------------------------------------

data HtmlElement
  = HtmlString String                    -- Plain text.
  | HtmlTag String [Attr] HtmlElements   -- Structured markup.
  deriving (Eq, Show)

type HtmlElements = [HtmlElement]

class HTML a where
  toHtml :: a -> HtmlElement

instance HTML Link where
  toHtml (Link hrf txt) = HtmlTag "a" [MkAttr "href" hrf] [HtmlString txt]

------------------------------------------------------
amountsEuro :: [Int]
amountsEuro = [1, 2, 5, 10, 20, 50, 100, 200]

changesEuro :: Int -> [[Int]]
changesEuro = changes amountsEuro

changes :: [Int] -> Int -> [[Int]]
changes _ 0  = [[]]
changes [] _ = []
changes (j:js) i
  | j > i = changes js i
  | otherwise 
  = [ j:xs | xs <- changes (j:js) (i - j)] ++ changes js i

------------------------------------------------------

--fold with double function
foldTree :: (a -> b) -> (b -> b -> b) -> (Tree a -> b)
foldTree f _ (Leaf x) = f x
foldTree f g (Fork t1 t2) = g (foldTree f g t1) (foldTree f g t2)

idTree :: Tree a -> Tree a
idTree = foldTree Leaf Fork

nrOfLeaves :: Tree a -> Int
nrOfLeaves = foldTree (\_ -> 1) (+)

sumTree :: Tree Int -> Int
sumTree = foldTree id (+)


------------------------------------------------------

iets :: Int->[Int]
iets x= [ y+z |y<- [0..x], z<-[0,1] ]
>[0,1,1,2,2,3,3,4]

------------------------------------------------------
applyAll :: [a -> a] -> a -> a
applyAll fs x = foldr ($) x fs 
-- same as: foldr (\f x -> f x) x fs
-- Alternatively, with primitive recursion:
--   applyAll []     x = x
--   applyAll (f:fs) x = f (applyAll fs x)

--myFoldr :: (a -> b -> b) -> b -> [a] -> b
--myFoldr _f base []     = base
--myFoldr  f base (x:xs) = f x (myFoldr f base xs)

-- 10/01/2021 a foldl would be more logic here
--   applyAll []     x = x
--   applyAll (f:fs) x = applyAll fs (f x)
--myFoldl :: (b -> a -> b) -> b -> [a] -> b
--myFoldl _f base []     = base
--myFoldl  f base (x:xs) = myFoldl f (f base x) xs


------------------------------------------------------
myRepeat :: Int -> Int -> [Int]
myRepeat n x = [ x | _i <- [1..n] ]
------------------------------------------------------
flatten :: [[Int]] -> [Int]
flatten xss = [ x | xs <- xss, x <- xs ]
------------------------------------------------------
myLast :: [Int] -> Int
myLast (n:ns) 
  | ns==[] =n
  | otherwise = myLast ns

