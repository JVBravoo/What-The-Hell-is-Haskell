ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a,b,c)    | a <= b && b <= c = (a,b,c)
                        | a <= b && b >= c && a <= c = (a,c,b)
                        | a <= b && b >= c && a >= c = (c,a,b)
                        | b <= a && a <= c = (b,a,c)
                        | b <= a && a >= c && b <= c = (b,c,a)
                        | b <= a && a >= c && b >= c = (c,b,a)

removePar :: [[Int]] -> [[Int]]
removePar [] = []
removePar (x:xs)    |(filter even x) /= []  = [x] ++ removePar xs
                    |otherwise = removePar xs

data List t = Nil | Cons t (List t)
            deriving(Show)

fromList :: [t] -> List t
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)