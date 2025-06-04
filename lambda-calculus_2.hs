import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void)
import System.IO

type VarName = String

data LambdaTerm
    = Lam VarName LambdaTerm
    | App LambdaTerm LambdaTerm
    | Var VarName              
    deriving Eq

instance Show LambdaTerm where
    show (Var x) = x
    show (Lam x m) = "(lambda " ++ x ++ " . " ++ show m ++ ")"
    show (App m n) = "(" ++ show m ++ " " ++ show n ++ ")"

substitute :: LambdaTerm -> VarName -> LambdaTerm -> LambdaTerm
substitute (Var x) v t
    | x == v    = t
    | otherwise = Var x
substitute (Lam x m) v t
    | x == v    = Lam x m
    | otherwise = Lam x (substitute m v t)
substitute (App m n) v t = App (substitute m v t) (substitute n v t)

betaReduce :: LambdaTerm -> LambdaTerm
betaReduce (App (Lam x m) n) = substitute m x n
betaReduce (App m n) = App (betaReduce m) (betaReduce n)
betaReduce (Lam x m) = Lam x (betaReduce m)
betaReduce t = t

canBeReduced :: LambdaTerm -> Bool
canBeReduced (App (Lam _ _) _) = True
canBeReduced (App m n) = canBeReduced m || canBeReduced n 
canBeReduced (Lam _ m) = canBeReduced m
canBeReduced _ = False

reduceSubterms :: LambdaTerm -> LambdaTerm
reduceSubterms (App m n)
    | canBeReduced (App m n) = betaReduce (App m n)
    | otherwise = App (reduceSubterms m) (reduceSubterms n)
reduceSubterms (Lam x m) = Lam x (reduceSubterms m)
reduceSubterms t = t

eval :: LambdaTerm -> LambdaTerm
eval = eval0 0

eval0 :: Int -> LambdaTerm -> LambdaTerm
eval0 c term
    | canBeReduced term && c < maxCount = eval0 (c + 1) (reduceSubterms term)
    | otherwise = term

maxCount :: Int
maxCount = 1000

-- Парсер
spaces' :: Parser ()
spaces' = skipMany (oneOf " \t\n")

symbol :: String -> Parser String
symbol s = spaces' *> string s <* spaces'

parseVarName :: Parser VarName
parseVarName = spaces' *> many1 letter <* spaces'

parseVar :: Parser LambdaTerm
parseVar = Var <$> parseVarName

parseAbs :: Parser LambdaTerm
parseAbs = do
    _ <- spaces' *> char '('
    _ <- oneOf "\\|"
    x <- parseVarName
    _ <- symbol "."
    body <- parseExpr
    _ <- char ')'
    return $ Lam x body

parseApp :: Parser LambdaTerm
parseApp = do
    _ <- spaces' *> char '('
    e1 <- parseExpr
    e2 <- parseExpr
    _ <- char ')'
    return $ App e1 e2

parseExpr :: Parser LambdaTerm
parseExpr = try parseAbs <|> try parseApp <|> parseVar

parseLambdaTerm :: String -> LambdaTerm
parseLambdaTerm s =
    case parse parseExpr "" s of
        Left err -> error (show err)
        Right t  -> t

main :: IO ()
main = do
    putStrLn "Введите |-терм:"
    hFlush stdout
    line <- getLine
    let term = parseLambdaTerm line
        result = eval term
    putStrLn $ "Результат: " ++ show result
    main

-- ((\x.(x y)) z) --> (z y)
