type VarName = String

data LambdaTerm
    = Lam VarName LambdaTerm
    | App LambdaTerm LambdaTerm
    | Var VarName              

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

maxCount = 1000

testTerm1 = Lam "n" (App (Var "f") (Var "n"))
testTerm2 = App testTerm1 (Lam "a" (Var "a"))
testTerm3 = App (Lam "x" (App (Var "x") (Var "x"))) (Var "n")
xx        = Lam "x" (App (Var "x") (Var "x"))
testTerm4 = App xx xx
t         = Lam "x" $ App (Var "f") (App (Var "x") (Var "x"))
yComb     = Lam "f" (App t t)
testTerm5 = App yComb (Var "foo")

id' = Lam "x" (Var "x")

new = App testTerm1 id'

term :: LambdaTerm
term = App 
        (Lam "x" (Var "z")) 
        (App 
            (Lam "w" (App (App (Var "w") (Var "w")) (Var "w"))) 
            (Lam "w" (App (App (Var "w") (Var "w")) (Var "w")))
        )


-- eval(term) -> z


