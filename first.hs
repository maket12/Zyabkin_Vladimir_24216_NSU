hello = "Hello world"

a = 1 + 2


add :: Int -> Int -> Int
add x y = x + y

-- :t add

-- Bool: True or False

-- is_Int :: Int -> Bool
-- is_Int n = n == 42 

int2str :: Int -> String
int2str 1 = "1"
int2str 2 = "2"
int2str 3 = "3"
int2str 4 = "4"
int2str 5 = "5"
int2str n = "I don't know what to say"

str2int :: String -> Int
str2int n = case n of
    "One" -> 1
    "Two" -> 2
    "Three" -> 3
    "Four" -> 4
    "Five" -> 5
    _ -> -1


condition :: Int -> Int -> String
condition a b | a < b = "Less"
              | a == b = "Equal"
              | otherwise = "Great"



f :: Float -> Float
f x = x * 2
g :: Float -> Float
g x = x * 3



func :: (Float -> Float) -> (Float -> Float) -> Float -> Float
func f g p = max (f p) (g p)
