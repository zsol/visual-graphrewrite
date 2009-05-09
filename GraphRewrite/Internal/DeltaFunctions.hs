module GraphRewrite.Internal.DeltaFunctions
where

data Prec
    = Infixr Int
    | Prefix

deltaNames :: [(Prec, String)]
deltaNames = [(Infixr 5, "++"), (Infixr 5, ":")] ++ map p ["div", "mod", "eqInt", "not", "Cons", "Nil", "succ", "True", "False", "sin", "[]"]  where

    p s = (Prefix, s)

evalDelta :: String -> [String] -> String
evalDelta "sin" [x] = show $ sin $ (read x :: Double)
evalDelta "True" [] = "True"
evalDelta "False" [] = "False"
evalDelta "succ" [n] = show $ (read n) + (1 :: Int)
