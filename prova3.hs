myMap f [] = []
myMap f xs = myMap' f xs [] 

myMap' f [] acc = acc
myMap' f (x:xs) acc = myMap' f xs (acc++[f x])