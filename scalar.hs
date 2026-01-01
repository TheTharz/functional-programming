scalar :: Num a => [a] -> [a] -> a
scalar xs ys = sum [x*y|(x,y)<-zip xs ys]