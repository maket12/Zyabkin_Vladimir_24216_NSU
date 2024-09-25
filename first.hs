f :: Double -> Double
f x = (x + 2) * 8

func :: (Double -> Double) -> Double -> Double
func f p = max (exp p) (f p) 

-- func f 10  --> 22026.46579.... (it means that exp(p) returns max value)
-- func f 1  --> 24.0 (it means that f(p) returns max value)
-- func f 3  --> 40.0 (it means the same ^^^^)
