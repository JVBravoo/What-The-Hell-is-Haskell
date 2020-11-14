data Dict k v = Dict [(k,v)]
            deriving(Show)

find :: (Eq m) => Dict m n -> m -> Maybe n
find (Dict []) _ = Nothing
find (Dict ((k,v):xs)) nk  | (k == nk) = Just v
                           | otherwise = find (Dict xs) nk

instance Functor (Dict m) where
    fmap f (Dict xs) = Dict (map (fmap f) xs)

addDic :: Dict m n -> (m,n) -> Dict m n
addDic (Dict xs) (k,v) = Dict ((k,v):xs)x

update :: (Eq v, Eq k) => Dict k v -> Dict k v -> Dict k v
update (Dict []) (Dict another) = Dict another
update (Dict j) (Dict []) = Dict ino
update (Dict ((k,v):ino)) (Dict another)|find (Dict another) k == Nothing = (addDic (update (Dict ino) (Dict another)) (k,v))
                                    |otherwise = update (Dict ino) (Dict another)