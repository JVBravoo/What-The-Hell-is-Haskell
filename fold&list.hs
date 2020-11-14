reverse1 :: [t] -> [t]
reverse1 [] = []
reverse1 xs =  foldr (\x s-> s ++ [x]) [] xs

remove :: Eq t => [t] -> [t] -> [t]
remove _ [] = []
remove [] [x] = [x]
remove xs ys = foldr (\x acc -> if elem x xs then acc else x : acc) [] ys

sumsq :: Int -> Int
sumsq y = foldr (\x acc-> x^2 + acc) 0 [1..y]

myFilter :: (t -> Bool) -> [t] -> [t]
myFilter f [] = []
myFilter f xs = foldr (\x acc-> if (f x) == False then acc else x : acc) [] xs

myLength :: [t] -> Int
myLength [] = 0
myLength xs = foldr (\x acc-> 1 + acc) 0 xs

minList :: [Int] -> Int
minList [] = 0
minList xs = foldr1 (\x y-> if x >= y then y else x) xs