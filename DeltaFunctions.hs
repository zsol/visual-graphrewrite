module DeltaFunctions
where

deltaNames = ["++", "div", "mod", "eqInt", "not", "Cons", "Nil", "succ", "True", "False", "sin"]

evalDelta :: String -> [String] -> String
evalDelta "sin" [x] = show $ sin $ read x
evalDelta "True" [] = "True"
evalDelta "False" [] = "False"
evalDelta "succ" [n] = show $ (read n) + 1