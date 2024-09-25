f :: Double -> Double
f x = x + 3 + cos (pi/3)


func :: (Double -> Double) -> Double -> Int -> Double
func f p n | n == 0 = p
           | n < 0 = error "n can't be negative!"
           | otherwise = func f (f p) (n - 1)

-- func f 2 3  --> 12.5 (plus three times per digit 3 and also three times per 0.5)
-- func f 2 0  --> 2.0
-- func f 8 (-5) --> (current error message)
