import Data.List

data List t = Nil | Cons t (List t)
            deriving(Eq,Show)

data Tree t = NilT | Node t (Tree t) (Tree t)
            deriving(Eq,Show)

concatList :: Ord t => List t -> t -> List t
concatList Nil x = (Cons x Nil)
concatList (Cons x a) b = Cons x (concatList a b)

depth :: Ord t => Tree t -> Int
depth NilT = 0
depth (Node a NilT NilT) = 0
depth (Node a b c)  |(depth b) >= (depth c) = 1 + (depth b)
                    |otherwise = 1 + (depth c)

collapse :: Ord t => Tree t -> [t]
collapse NilT = []
collapse (Node a b c) = collapse b ++ [a] ++ collapse c

reverseList :: Ord t => List t -> List t
reverseList Nil = Nil
reverseList (Cons a b) = concatList((reverseList b)) a