-- | This module contains some pre-defined functions (called delta functions, hence the name), some properties and how to evaluate them.
module GraphRewrite.Internal.DeltaFunctions
where

data Prec
    = Infixr Int
    | Prefix

-- | Names and 'Prec' of the delta functions.
deltaNames :: [(Prec, String)]
deltaNames = [(Infixr 5, "++"), (Infixr 5, ":"), (Infixr 6, "+"), (Infixr 6, "-"), (Infixr 6, "*")] ++
             map p ["div", "mod", "eqInt", "not", "Tuple", "succ", "True", "False", "sin", "[]"]  where

    p s = (Prefix, s)

-- | How to evaluate a delta function.
evalDelta
    :: String -- ^ Function name.
    -> [String] -- ^ Function arguments in 'String' representation.
    -> String -- ^ Result, again in 'String' format.
evalDelta "sin" [x] = show $ sin (read x :: Double)
evalDelta "True" [] = "True"
evalDelta "False" [] = "False"
evalDelta "succ" [n] = show $ read n + (1 :: Int)
evalDelta "++" [a,b] = a ++ b
evalDelta "+" [a,b] = show $ (read a :: Int) + (read b :: Int)
evalDelta "-" [a,b] = show $ (read a :: Int) - (read b :: Int)
evalDelta "*" [a,b] = show $ (read a :: Int) * (read b :: Int)
evalDelta f _ = error $ "Evaluating this function is NYI: " ++ f
