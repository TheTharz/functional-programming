perfect::Int->Bool
perfect n = sum [x | x<-[1..n-1],n `mod` x == 0] == n
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]