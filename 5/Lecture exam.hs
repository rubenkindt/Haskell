data Turtle =
  Empty |  Angle Double Turtle | Distance Double Turtle


done :: Turtle 
done = Empty turn :: Double -> Turtle
turn t = Angle t Emptystep :: Double -> Turtle 
step t = Distance t Empty

(>>>) :: Turtle -> Turtle -> Turtle 
Empty >>> t            = t
(Angle x y) >>> z      = Angle x (y >>> z)
(Distance x y) >>> t   = Distance x (y>>> z)

{-
zou ook werken
(>>>) :: Turtle -> Turtle -> Turtle
(>>>) Empty tur2= tur2
(>>>) (Dist a tur1) tur2= Dist a ((>>>) tur1 tur2)
(>>>) (Angle a tur1) tur2= Angle a ((>>>) tur1 tur2)
-}

square :: Turtle 
square = (Distance 50 Empty) >>> (Angle 90 Empty) >>> 
         (Distance 50 Empty) >>> (Angle 90 Empty) >>> 
         (Distance 50 Empty) >>> (Angle 90 Empty) >>>
         (Distance 50 Empty) >>> (Angle 90 Empty)

{-
square :: Turtle
square = side >>> side >>> side >>> side
   where side = step 50 >>> turn 90
   
square :: Turtle
square = foldr1 (>>>) . intersperse (turn 90)  . replicate 4 $ step 50
-}


turtleToLines :: Turtle -> [Line]
turtleToLines = turtleToLines' (500, 500) 0turtleToLines' :: Point -> Double -> Turtle -> [Line]
turtleToLines' _ _ Done = []
turtleToLines' start d (Turn n t) = turtleToLines' start (d + n) t
turtleToLines' start@(x,y) d (Step l t) = let x' = x + l * sin (d * 2 * pi / 360) -- start@(x,y) let's you use start in plaats van (x,y)
                                              y' = y + l * cos (d * 2 * pi / 360)
                                              end = (x',y') in
                                                  (start, end) : turtleToLines' end d t

{-
turtleToLines :: Turtle -> [Line]
turtleToLines = go (500,500) 0
    where
        go _ _ Done                 = []
        go start d (Turn n t)       = go start (d + n) t
        go start@(x,y) d (Step l t) = let x' = x + l * sin (d * 2 * pi / 360)
                                          y' = y + l * cos (d * 2 * pi / 360)
                                          end = (x',y') in
                                              (start, end) : go end d t
-}


{-
linesToSVG :: [Line] -> String
linesToSVG list = startOfSvg ++ "\n" ++ unlines (map lineToSVGString list )  ++ "\n" ++ endOfSvgstartOfSvg :: String
startOfSvg = "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"lineToSVGString :: Line -> String 
lineToSVGString ( (x1,y1) , (x2,y2) ) = "< line x1=" ++ show x1 ++ " y1=" ++ show y1 ++ " x2=" ++ show x2 ++ " y2=" ++ show y2 ++ "stroke=\"blue\" stroke-width=\"4\" />" endOfSvg :: String
endOfSvg = "</svg>"
-}


writeSVG :: FilePath -> Turtle -> IO ()
writeSVG fp = writeFile fp . linesToSVG . turtleToLines

fdone :: Fractal
fdone = FDonefturn :: Double -> Fractal 
fturn = FTurn FDonefstep :: Fractal
fstep a = FStep a FDone>-> :: Fractal -> Fractal -> Fractal
Done >-> t = t
(Step a t) >-> s = Step a  (t >->  s)
(Turn a t) >-> s = Turn a (t >-> s)

data Fractal = FDone | FStep Fractal | FTurn Double Fractal
data Fractal = FDone | FTurn Double Fractal | FStep Fractal

fdone :: Fractal
fdone = FDonefturn :: Double -> Fractal
fturn n = FTurn n FDonefstep :: Fractal
fstep = FStep FDone(>->) :: Fractal -> Fractal -> Fractal
FDone >-> r       = r
(FTurn n f) >-> r = FTurn n (f >-> r)
(FStep f) >-> r = FStep (f >-> r)


concretize :: Double -> Fractal -> Turtle 
concretize _ FDone = Done
concretize a (FStep t) = Step a (concretize a t)
concretize a (FTurn b t) = Turn b (concretize a t) 

concretize :: Double -> Fractal -> Turtle
concretize _ FDone       = Done
concretize l (FTurn n f) = Turn n (concretize l f)
concretize l (FStep f)   = Step l (concretize l f)

refine :: Fractal -> Fractal -> Fractal 
refine expansion FDone = FDone
refine expansion (FTurn a t) = FTurn a (refine t)
refine expansion (FStep t) = expansion >-> refine expansion t
refine :: Fractal -> Fractal -> Fractal

refine _ FDone               = FDone
refine expansion (FTurn n f) = FTurn n (refine expansion f)
refine expansion (FStep f)   = expansion >-> refine expansion f

times :: Int -> (a -> a) -> (a -> a)
times 0 = id
times n f = times (n-1) f . f

times :: Int -> (a -> a) -> (a -> a)
times n = foldr (.) id . replicate n

exam :: Fractal -> Fractal -> Int -> Double -> FilePath -> IO ()
exam program expansion n d filename = writeSVG fileName (concretize (times n (refine program expansion) ) )

exam :: Fractal -> Fractal -> Int -> Double -> FilePath -> IO ()
exam prog exp n d fp = writeSVG fp . concretize d . times n (refine exp) $ prog

main :: IO ()
main = exam program expansion 2 10 "/path/to/somewhere.svg"