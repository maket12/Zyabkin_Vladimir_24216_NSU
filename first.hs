even' :: Int -> Bool
even' x =  mod x 2 == 0

-- even' 0  --> True
-- even' 3  --> False

odd' :: Int -> Bool
odd' x =  mod x 2 /= 0

-- even2' 0  --> False
-- even2' 3  --> True

fibonacci' :: Int -> Int
fibonacci' x | x <= 0 = error "Index must be positive!"
             | x == 1 = 0
             | x == 2 = 1
             | otherwise = fibonacci' (x - 1) + fibonacci' (x - 2)

-- fibonacci' 4 --> 2
-- fibonacci' 8 --> 13

sumOddFibonacci :: Int -> Int
sumOddFibonacci x = sum [fib n | n <- [1..x], odd (fib n)]
    where
        fib 1 = 0
        fib 2 = 1
        fib n = fib (n-1) + fib (n-2)

-- sumOddFibonacci 7 --> 10
-- sumOddFibonacci 10 --> 44

computeMagicNumber :: Int -> Int
computeMagicNumber x = sum (digits x)
    where
        digits n
            | n == 0 = []
            | otherwise = digits (n `div` 10) ++ [n `mod` 10]

-- computeMagicNumber 2674 --> 19
-- computeMagicNumber 267 --> 15
