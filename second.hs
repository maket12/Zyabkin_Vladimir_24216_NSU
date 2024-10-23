reverse' :: Eq a => [a] -> [a]
reverse' = foldl (\acc x -> [x] ++ acc) [] 

-- reverse' [1, 4, 5] --> [5,4,1]

evenOnly :: [a] -> [a]
evenOnly = snd . foldl (\(counter, acc) x -> 
                    if counter `mod` 2 == 0 
                    then (counter + 1, acc ++ [x]) 
                    else (counter + 1, acc)) (1, []) 

-- evenOnly [1..10] --> [2,4,6,8,10]

for :: (Int, a) -> (Int -> Int) -> (Int -> Bool) -> (Int -> a -> a) -> a
for (i, acc) inc pred body =
  if pred i
  then for (inc i, body i acc) inc pred body
  else acc

sum' :: Num a => [a] -> a
sum' lst =
  for (0, 0) (\i -> i + 1) (\i -> i < length lst) (\i acc -> acc + lst!!i)

-- sum' [1, 5, 8] --> 14 

concatAll :: [String] -> String
concatAll strs = for (0, "") (\i -> i+1) (\i -> i < length strs) (\i acc -> acc ++ strs!!i)

-- concatAll ["hi", "good"] --> "higood"


numberMultiply :: a -> [b] -> [(a, b)]
numberMultiply x = foldl (\acc e -> acc ++ [(x, e)]) [] 


decartMultiply :: [a] -> [b] -> [(a, b)]
decartMultiply xs ys = foldl (\acc e -> acc ++ numberMultiply e ys) [] xs

-- decartMultiply [1, 2] ['a', 'b', 'c'] --> [(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c')]

-- Бинарное отношение
type BinaryRelation a = [(a, a)]

-- Проверка на рефлексивность
refl :: Eq a => [a] -> BinaryRelation a -> Bool
refl m rel = all (\x -> (x, x) `elem` rel) m

-- Проверка на симметричность
sim :: Eq a => [a] -> BinaryRelation a -> Bool
sim _ rel = all (\(x, y) -> (y, x) `elem` rel) rel

-- Проверка на транзитивность
trans :: Eq a => [a] -> BinaryRelation a -> Bool
trans _ rel = all (\(x, y) -> all (\z -> not ((y, z) `elem` rel) || (x, z) `elem` rel) (map snd rel)) rel

-- trans [1,2,3] [(1,2), (2,3), (1,3)] --> True