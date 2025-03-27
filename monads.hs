import Control.Monad.State

data Person = Person {
    name :: String,
    surname :: String,
    father :: Maybe Person,
    mother :: Maybe Person
} deriving Show


-- examples

jane = Person "Jane" "Smith" Nothing Nothing
russ = Person "Russ" "Cock"


mothersFather :: Person -> Maybe Person
mothersFather p = case mother p of
    Nothing -> Nothing
    Just mom -> father mom

mothersFather' :: Person -> Maybe Person
mothersFather' p = do
    mom <- mother p
    father mom


hasGrandParents :: Person -> Maybe Person
hasGrandParents p = case father p of
    Nothing -> Nothing
    Just dad -> case mother dad of
        Nothing -> Nothing
        Just grandMother -> case mother p of
            Nothing -> Nothing
            Just mom -> case father mom of
                Nothing -> Nothing
                Just grandFather -> Just grandMother

hasGrandParents' :: Person -> Maybe Person
hasGrandParents' p = do
    dad <- father p
    grandMother <- mother dad
    mom <- mother p 
    grandFather <- father mom 
    return grandMother


sumTwoInts :: IO ()
sumTwoInts = readLn >>= \x -> readLn >>= \y -> print (x + y)


fact' :: State (Int, Int) Int
fact' = do
    (n, acc) <- get
    if n == 0
        then return acc
        else do
            put (n - 1, acc * n)
            fact'

fact :: Int -> Int
fact n = evalState fact' (n, 1)


fibb' :: State (Int, Int, Int) Int
fibb' = do
    (step, n1, n2) <- get 
    if step == 0
        then return n1 
        else do
            put (step - 1, n1 + n2, n1)
            fibb'

fibb :: Int -> Int
fibb n = evalState fibb' (n, 1, 0) 


data BinTree a = Nil | Node (BinTree a) a (BinTree a) deriving Show

numberTree :: BinTree () -> BinTree Integer
numberTree tree = evalState (numberTree' tree) 0

numberTree' :: BinTree () -> State Integer (BinTree Integer)
numberTree' Nil = return Nil
numberTree' (Node left _ right) = do
    left' <- numberTree' left
    currentNumber <- get
    put (currentNumber + 1)
    right' <- numberTree' right

    return (Node left' currentNumber right')
