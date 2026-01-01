isPrime :: Int -> Bool
isPrime n | n<2 = False
  | otherwise = null [x |x<-[2..n-1], n `mod` x == 0]

uptoPrimes :: Int -> [Int]
uptoPrimes n = [x | x<-[2..n], isPrime x]

triplets :: Int -> [(Int, Int, Int)]
triplets n = [(x,y,z) | (x,y,z) <- zip3 ps (tail ps) (tail (tail ps))] where ps = uptoPrimes n