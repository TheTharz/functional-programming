secret :: String
secret = "haskell"

display :: [Char] -> String
display guessed =
  [ if c `elem` guessed then c else '_' | c <- secret ]

isFinished :: [Char] -> Bool
isFinished guessed = all (`elem` guessed) secret

play :: [Char] -> IO ()
play guessed = do
  putStrLn (display guessed)
  if isFinished guessed then do
    putStrLn "You win!"
  else do
    putStrLn "Enter a character: "
    c <- getChar
    putStrLn ""
    play (guessed ++ [c])

main :: IO ()
main = play []
