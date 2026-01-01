primes = sieve [2..]

sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

twin (x,y) = y == x+2

twins = filter twin (zip primes (tail primes))