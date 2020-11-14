import Data.List

ordenar :: [Int] -> [Int]
ordenar xs = sortBy (\l r-> (compare (sumDigits l) (sumDigits r))) xs

sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits y = (y `mod` 10) + sumDigits (y `div` 10)

palindromo :: Eq t => [t] -> Bool
palindromo [] = True
palindromo xs   |(reverse xs) == xs = True
                |otherwise = False

ordenaTripla :: (Int,Int,Int) -> (Int,Int,Int)
ordenaTripla (a,b,c)| a <= b && b <= c = (a,b,c)
                    | a <= b && b >= c && a <= c = (a,c,b)
                    | a <= b && b >= c && a >= c = (c,a,b)
                    | b <= a && a <= c = (b,a,c)
                    | b <= a && a >= c && b <= c = (b,c,a)
                    | b <= a && a >= c && b >= c = (c,b,a)