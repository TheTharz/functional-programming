type MyString = [Char]

word :: MyString
word = ['a', 'b', 'c', 'd']

type Position = (Int, Int)
origin :: Position
origin = (0, 0)

moveLeft :: Position -> Position
moveLeft (x,y) = (x - 1, y)

type Pair a = (a, a)
mult :: Pair Int -> Int
mult (m,n) = m*n

data Color = Red | Green | Blue

data Answer = Yes | No | Unknown
  deriving (Show) --User-defined data types are not printable by default

myFlip :: Answer -> Answer
myFlip Yes = No
myFlip No = Yes
myFlip Unknown = Unknown

ans :: Answer
ans = Yes

data Shape = Circle Float | Rectangle Float Float | Square Float | Triangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rectangle w h) = w * h
area (Square w) = w^2
area (Triangle b h) = 0.5 * b * h

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

data Nat = Zero | Succ Nat
  deriving (Eq, Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n) 

ones = 1 : ones

replicatee :: Int -> a -> [a]
replicatee 0 _ = []
replicatee n x = x : replicatee (n-1) x

oddNumbers n = map f [0..n-1]
          where 
            f x = x*2+1

odds n = map (\x->x*2+1) [0..n-1]

test :: [Float]
test = [1..20]

successor = map (^3) test

concatenate :: [[a]] -> [a]
concatenate xs = [a | x<-xs, a<-x]

factors :: Int -> [Int]
factors n = [x | x<-[1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x<-[2..n], prime x]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x<=y|(x,y)<- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions n xs = [i | (x,i) <- zip xs [0..] , x == n]

countOccurance :: Char -> String -> Int
countOccurance n xs = length [x|x<-xs,x==n]