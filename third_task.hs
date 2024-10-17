import GHC.Real (infinity)
fibonacci' :: Int -> Int
fibonacci' x | x <= 0 = error "Index must be positive!"
             | x == 1 = 0
             | x == 2 = 1
             | otherwise = fibonacci' (x - 1) + fibonacci' (x - 2)

-- fibonacci' 4 --> 2
-- fibonacci' 8 --> 13

fibmod5 :: [Int]
fibmod5 = [fibonacci' x | x <- [1..], fibonacci' x `mod` 5 == 0]

-- ghci> take 7 fibMod5 --> [0,5,55,610,6765,75025,832040]
-- Во второй и последующие разы ответ выдавался практически моментально.
-- Видимо, при первом запуске функции, ответ как раз-таки вычислялся на моменте вызова функции.
-- В последующих запусках в памяти уже хранился результат вызова

distance :: (Floating a) => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

perimetr :: [(Double, Double)] -> Double
perimetr [p1, p2, p3] = distance p1 p2 + distance p2 p3 + distance p3 p1

-- perimetr [(0, 0), (4, 0), (0, 3)] --> 12.0

checkAllEq :: Eq a => [a] -> Bool
checkAllEq x | null x = True
             | length x == 1 = True
             | otherwise = all (== head x) (tail x)

-- checkAllEq [1, 1, 1] --> True
-- checkAllEq [1, 2] --> False

findMin :: [(Double, Double)] -> Double
findMin [] = 0
findMin [_] = 1 / 0
findMin (x:y:xs) = min (distance x y) (findMin (y:xs))

-- findMin [(1, 1), (4, 5), (6, 8), (1, 3)] --> 3.605551275463989

-- Функция, которая выполняет одну инструкцию
applyInstruction :: String -> Double -> Double
applyInstruction "inc" n = n + 1
applyInstruction "dec" n = n - 1
applyInstruction "double" n = n * 2
applyInstruction "sqrt" n = sqrt n
applyInstruction "halveIfPositive" n = if n > 0 then n / 2 else n
applyInstruction _ n = n  -- Если инструкция неизвестна, оставляем число без изменений

-- Основная функция для последовательного выполнения инструкций
computeProgram :: [String] -> Double -> Double
computeProgram program p = foldl (flip applyInstruction) p program

-- computeProgram ["inc", "dec", "inc", "double", "halveIfPositive"] (-51) --> -100.0

