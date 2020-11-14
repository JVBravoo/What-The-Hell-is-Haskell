filter1 p xs = [x | x <- xs, p x]
--[x | x <- [1..10], odd x]

biggerThan :: [[Int]] -> Int -> [[Int]]
biggerThan xs a = filter (/=[]) (map (\x -> if (length x) > a then x else []) xs)

funcaoInversa f a b = (\x y z -> x z y) f a b
--(\x y z -> x z y) (-) 4 5

data ArvoreBinaria a = Folha a | Nodo (ArvoreBinaria a) (ArvoreBinaria a)
           deriving(Show)

instance Functor ArvoreBinaria where
    fmap f (Folha x) = Folha (f x)
    fmap f (Nodo y z) = Nodo (fmap f y) (fmap f z)