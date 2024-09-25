f :: Float -> Float
f x = x * 2
g :: Float -> Float
g x = x * 3



func :: (Float -> Float) -> (Float -> Float) -> Float -> Float
func f g p = max (f p) (g p)

-- func f g 10  --> 30.0 (it means that g - max)
-- func f g 17  --> 51.0 (it means the same ^^^)
