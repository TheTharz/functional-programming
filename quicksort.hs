import Data.Text.Internal.Fusion.Size (larger)
qsort ::Ord a => [a] -> [a]

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                  smaller = [a|a<-xs,a<=x]
                  larger = [a|a<-xs,a>x]