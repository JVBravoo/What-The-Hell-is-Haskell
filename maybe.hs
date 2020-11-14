data Tree a = Node (Maybe (a, Tree a, Tree a))
            deriving (Show)

instance Functor Tree where
    fmap f (Node (Nothing)) = Node (Nothing)
    fmap f (Node (Just (a,b,c))) = Node (Just ((f a),(fmap f b), (fmap f c)))

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just a) f = f a