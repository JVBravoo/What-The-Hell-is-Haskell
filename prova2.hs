remove :: Int -> [[Int]] -> [[Int]]
remove _ [[]] = []
remove a xs = filter (/=[]) (map (\x-> if (foldr (+) 0 x) > a then x else []) xs)