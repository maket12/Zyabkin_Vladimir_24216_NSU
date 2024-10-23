func :: Int -> Int
func x = if even x then x + 1 else x

map' :: (a -> b) -> [a] -> [b]
map' f = foldl (\acc c -> acc ++ [f c]) []

-- map' func [1, 2, 4, 9] --> [1,3,5,9]

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> [f x] ++ acc) []

-- map'' func [1, 2, 4, 9] --> [1,3,5,9]


nub' :: Eq a => [a] -> [a]
nub' = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) []

-- nub' [1, 2, 2, 3, 3, 3, 4, 1] --> [1, 2, 3, 4]

union' :: Eq a => [a] -> [a] -> [a]
union' x y = nub' (x ++ y)

-- union' [1, 2, 4] [5, 7, 12, 14, 14, 2] --> [1,2,4,5,7,12,14]

intersection' :: Eq a => [a] -> [a] -> [a]
intersection' x y = foldl (\acc e -> if e `elem` y then acc ++ [e] else acc) [] x

-- intersection' [14, 10, 5] [14, 5, 8] --> [14,5]


computeProgram :: [String] -> Double -> Double
computeProgram [] value = value
computeProgram (inst: insts) value | inst == "inc" = computeProgram insts (value + 1)
                                   | inst == "dec" = computeProgram insts (value - 1)
                                   | inst == "double" = computeProgram insts (value * 2)
                                   | inst == "sqrt" = computeProgram insts (sqrt value)
                                   | inst == "halveIfPositive" = computeProgram insts (if value > 0 then value / 2 else value)

computeVector :: [String] -> [Double] -> [Double]
computeVector insts = foldl (\acc e -> acc ++ [computeProgram insts e]) []

-- computeVector ["inc", "dec", "inc", "double"] [1, 2, 3, 4] --> [4.0,6.0,8.0,10.0]

cleaner :: [String] -> [String]
cleaner insts = foldl (\acc e -> if e `elem` ["inc", "dec", "double", "sqrt", "halveIfPositive"] then acc ++ [e] else acc) [] insts

-- cleaner ["inc", "unknown", "double", "bad", "sqrt"] --> ["inc","double","sqrt"]

optimizer :: [String] -> [String]
optimizer [] = []
optimizer (inst:insts) = clean Nothing insts
    where
        clean :: Maybe String -> [String] -> [String]
        clean _ [] = []
        clean Nothing (x:xs) = x : clean (Just x) xs
        clean (Just prev) (x:xs) 
            | (prev == "inc" && x == "dec") || (prev == "dec" && x == "inc") = clean Nothing xs
            | otherwise = prev : clean (Just x) xs

-- optimizer ["inc", "inc", "dec", "double"] --> ["inc","double"]

f :: Double -> Double
f x = x * x

checkUpper :: (Double -> Double) -> [(Double, Double)] -> [(Double, Double)]
checkUpper f points = filter (\(x, y) -> f x < y) points

-- checkUpper f [(1.0, 2.0), (2.0, 5.0), (3.0, 8.0), (4.0, 15.0)] --> [(1.0,2.0),(2.0,5.0)]
