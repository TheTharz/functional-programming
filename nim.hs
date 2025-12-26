import Data.Char
import Data.Bifoldable (Bifoldable)

-- board utilities

type Board = [Int]
initial :: Board
initial = [1,2,3,4,5]

finished :: Board -> Bool
finished b = all (== 0) b

valid :: Board -> Int -> Int -> Bool
valid board row amount = board !! (row - 1) >= amount

move :: Board -> Int -> Int -> Board
move board row amount = [adjust r n | (r,n) <- zip [1..5] board]
                        where adjust r n = if r == row then n - amount else n

-- I/O Utilities

newline :: IO()
newline = putChar '\n'

stars :: Int -> String
stars n = concat (replicate n "* ")

putRow :: Int -> Int -> IO()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStr (stars num)
  newline

putBoard :: Board -> IO()
putBoard board = sequence_ [putRow r n | (r,n) <- zip [1..5] board]

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x then
    return (digitToInt x)
  else
    do newline 
       putStrLn "ERROR: Invalid digit"
       getDigit prompt

-- Nim game
nim :: IO()
nim = play initial 1

next :: Int -> Int
next 1 = 2
next 2 = 1

play :: Board-> Int -> IO()
play board player = do
  newline
  putBoard board
  if finished board then
    do newline
       putStrLn $ "Player " ++ show (next player) ++ " wins!"
  else do
    newline
    putStr ("Player " ++ show player ++ " playing")
    newline
    r <- getDigit "Enter a row: "
    n <- getDigit "Number of stars to remove: "
    if valid board r n then
      play (move board r n) (next player)
    else do
      newline
      putStrLn "ERROR: Invalid move"
      play board player