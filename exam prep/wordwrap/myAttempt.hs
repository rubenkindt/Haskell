-- YOUR NAME
-- R-NUMBER
-- MAJOR
module MyHaskell where

-- 1.1
data LineItem = S
  | N
  | W String
    deriving (Eq)
              -- REMOVE Show after implementing 1.3
              --Show
             --)

-- 1.2
mkSpace :: LineItem
mkSpace = S

mkNewline :: LineItem
mkNewline = N

mkWord :: String -> LineItem
mkWord = W

-- 1.3
-- Comment out and complete this code
instance Show LineItem where
 show S = show " "
 show N = show "\n"
 show (W str) = show str
 

-- 1.4
toLineItems :: String -> [LineItem]
toLineItems = mergeWords . map (\x -> char2LineItem [x]) 

mergeWords :: [LineItem] -> [LineItem]
mergeWords [] = []
mergeWords (S:xs) = (S:mergeWords xs)
mergeWords (N:xs) = (N:mergeWords xs)
mergeWords ((W str):[]) = [(W (str))]
mergeWords ((W str):(W str2):xs) = mergeWords ((W (str ++ str2)):xs)
mergeWords ((W str):S:xs) = ((W str):S:mergeWords xs)
mergeWords ((W str):N:xs) = ((W str):N:mergeWords xs)

char2LineItem :: String -> LineItem
char2LineItem x
 | x == " "  = mkSpace
 | x == "\n" = mkNewline
 | otherwise = mkWord x 
 

-- 1.5
fromLineItems :: [LineItem] -> String
fromLineItems []      = []
fromLineItems (S:xs)      = " "  ++ fromLineItems xs
fromLineItems (N:xs)      = "\n" ++ fromLineItems xs
fromLineItems ((W str:xs))= str  ++ fromLineItems xs


-- 2.1
removeSpaces :: [LineItem] -> [LineItem]
removeSpaces [] = [] 
removeSpaces (S:xs) = removeSpaces xs
removeSpaces (N:xs) = (N:(removeSpaces xs))
removeSpaces ((W str):xs)= ((W str):(removeSpaces xs))


-- 2.2
splitInLines :: [LineItem] -> [[LineItem]]
splitInLines [] = [[]]
splitInLines (N:[])= [[],[]]
splitInLines (x:[])= [[x]]
splitInLines list = (before:(splitInLines after2))
 where 
  (before, after) = break (==N) list 
  after2 = drop 1 after

{-
splitInLines  = [[] | x<-list] 
splitInLines (N:xs) = ([]:(splitInLines xs))
splitInLines ((W str):xs)= ((W str):(splitInLines xs))
-}

-- 2.3
separateTooLongWords :: Int -> [LineItem] -> [[LineItem]]
separateTooLongWords _ [] = [[]]
separateTooLongWords nr list = filter (not . null) (before:longW:(separateTooLongWords nr after))
  where 
   (before,aft) = break (\(W str) -> (length str) > nr) list
   (longW,after) = splitAt 1 aft 


-- 2.4
wrap :: Int -> [LineItem] -> [[LineItem]]
wrap = error "beep"
{-
wrap_helper _ [] = [[]]
wrap _ list = [[]]

wrap_helper :: Int -> Int -> [LineItem] -> [[LineItem]]


wrap_helper maxi remaining (x:xs) = if maxi==remaining && (size x) > maxi then 
  ([x]:(wrap_helper maxi remaining xs))

wrap_helper maxi remaining list = (before:(separateTooLongWords maxi maxi after))
  (before,after) = break (\(W str) cum -> (length str) > remaining) list
-}

size :: LineItem -> Int
size (W str) = length str

-- 2.5
joinLineWithSpaces :: [LineItem] -> [LineItem]
joinLineWithSpaces [] = []
joinLineWithSpaces ((W str):[]) = [(W str)]
joinLineWithSpaces ((W str):xs) = ((W str):mkSpace:(joinLineWithSpaces xs))

-- 2.6
joinLinesWithNewlines :: [[LineItem]] -> [LineItem]
joinLinesWithNewlines []    = []
joinLinesWithNewlines (x:[])= x
joinLinesWithNewlines (x:xs)= (x++(mkNewline:(joinLinesWithNewlines xs)))

-- DO NOT CHANGE THIS FUNCTION
wordWrap :: Int -> String -> String
wordWrap lineWidth =
    fromLineItems .
    joinLinesWithNewlines .
    map joinLinesWithNewlines .
    map (map joinLineWithSpaces . concatMap (wrap lineWidth)) .
    map (separateTooLongWords lineWidth) .
    splitInLines .
    removeSpaces .
    toLineItems


-- 3.1
-- antwoord bevat Functor which we have not seen
getLines :: IO String
getLines = error "Not implemented"

-- 3.2
interactiveWrapper :: IO ()
interactiveWrapper = error "Not implemented"
