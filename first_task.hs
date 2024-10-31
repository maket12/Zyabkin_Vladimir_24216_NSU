f :: Float -> Float
f x = x * 2
g :: Float -> Float
g x = x * 3



func :: (Double -> Double) -> (Double -> Double) -> Double -> Double
func f g p = max (f p) (g p)

-- func f g 10  --> 30.0 (it means that g - max)
-- func f g 17  --> 51.0 (it means the same ^^^)

foo :: Double -> Double
foo x = (x + 2) * 8

some :: (Double -> Double) -> Double -> Double
some f p = func exp f p

-- some foo 10  --> 22026.46579.... (it means that exp(p) returns max value)
-- some foo 2  --> 32.0 (it means that foo(p) returns max value)
-- some foo 3  --> 40.0 (it means the same ^^^^)

f :: Double -> Double
f x = x + 3 + cos (pi/3)


func :: (Double -> Double) -> Double -> Int -> Double
func f p n | n == 0 = p
           | n < 0 = error "n can't be negative!"
           | otherwise = func f (f p) (n - 1)

-- func f 2 3  --> 12.5 (plus three times per digit 3 and also three times per 0.5)
-- func f 2 0  --> 2.0
-- func f 8 (-5) --> (current error message)
