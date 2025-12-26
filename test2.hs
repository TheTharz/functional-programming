calculateNormalTotal :: Float -> Float -> Int -> Float

calculateNormalTotal principle rate years = principle + (principle * rate * fromIntegral years)

calculateCompoundedTotal :: Float -> Float -> Int -> Float

calculateCompoundedTotal principle rate years = principle * (1 + rate) ^ fromIntegral years

main :: IO ()
main = do
  print $ calculateNormalTotal 1000 0.05 5
  print $ calculateCompoundedTotal 1000 0.05 5