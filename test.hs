import Data.Time.Format.ISO8601 (yearFormat)
sumAndSquares :: IO()

sumAndSquares = do
  let x = [1..100]

  putStrLn $ "Numbers from 1 to 100 " ++ show x

  let y = [a | a<-x, a `mod` 3 == 0 || a `mod` 5 ==0]

  putStrLn $ "NUmbers divisible by 3 or 5 " ++ show y

  putStrLn $ "Sum of numbers divisible by 3 or 5 " ++ show (sum y)

  let squares = [a^2 | a<-y]

  putStrLn $ "Sum of squares of numbers divisible by 3 or 5 " ++ show (sum squares)

  putStrLn $ "Both sums as a tuple " ++ show (sum y, sum squares)
main :: IO()
main = sumAndSquares