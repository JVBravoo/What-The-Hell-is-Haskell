import Data.Char

takeWhile1 :: (t -> Bool) -> [t] -> [t]
takeWhile1 f [] = []
takeWhile1 f (x:xs) | f x == True = [x] ++ takeWhile1 f xs
                    |otherwise = []

posicaoAlfabeto :: String -> [Int]
posicaoAlfabeto [] = []
posicaoAlfabeto xs = foldr (++) [] (map (\x -> [ord x - ord 'a' + 1]) xs)

removeOddOnly :: [[Int]] -> [[Int]]
removeOddOnly [] = []
removeOddOnly (x:xs)    |(filter even x) /= []  = [x] ++ removeOddOnly xs
                        |otherwise = removeOddOnly xs

biggestStringSize :: [String] -> Int
biggestStringSize [] = 0
biggestStringSize xs = maximum (map (\x -> foldr (+) 0 (map (\y -> 1) x)) xs)