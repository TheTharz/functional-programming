solveQuadratic :: Float -> Float -> Float -> (Float, Float)

solveQuadratic a b c = 
  let discriminant = b^2 - 4*a*c
  in if discriminant < 0
    then error "No real roots"
    else let root1 = (-b + sqrt discriminant) / (2*a)
             root2 = (-b - sqrt discriminant) / (2*a)
         in (root1, root2)
         
main :: IO ()
main = do
  let (r1, r2) = solveQuadratic 1 (-3) 2
  putStrLn $ "The roots are: " ++ show r1 ++ " and " ++ show r2