safeDiv :: Integral t=> Maybe t -> Maybe t -> Maybe t
safeDiv _ Nothing = Nothing
safeDiv Nothing _ = Nothing
safeDiv (Just x) (Just y)   | y /= 0 = Just (div x y)
                            | otherwise = Nothing
                            
safeTail [] = Nothing
safeTail (x:xs) = Just xs

applyMaybe :: Maybe a -> (a->Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

-- applyMaybe (applyMaybe (safeTail [2,3,4]) (\x-> Just (x++[42]))) (\x-> safeTail x)