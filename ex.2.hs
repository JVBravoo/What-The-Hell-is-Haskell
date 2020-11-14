fact 0 = 1
fact n = product [1..n]

fact2 0 = 1
fact2 n = foldl (*) 1 [1..n]

sumList :: [Int] -> Int -> Int
sumList [] acc = acc
sumList xs acc = sumList (init xs) (last xs + acc)

foldr3 f z [] = z 
foldr3 f z xs = foldr3 f (f (last xs) z) (init xs)

foldl2 f z [] = z
foldl2 f z (x:xs) = foldl f (f z x) xs

concatList :: [a] -> [a] -> [a] -> [a]
concatList [] ys acc = acc ++ ys
concatList xs [] acc = acc ++ xs
concatList (x:xs) (y:ys) acc = concatList xs ys (acc++[x]++[y])

splitWord :: String -> String -> [String]
splitWord "" a = [a]
splitWord (' ':xs) a = a : splitWord xs ""
splitWord (x:xs) a = splitWord xs (a++[x])