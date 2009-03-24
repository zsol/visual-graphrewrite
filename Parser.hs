import System.Environment --getArgs
import Language.Haskell.Parser -- parseModule
import IPPrint --pprint
--import DataGraph
import Language.Haskell.Syntax
import Rename
import Convert
import Data.Supply
import Data.Map
import RewriteApp
import RewriteAppTypes

predef :: [String]
predef = ["++", "div", "mod", "eqInt", "not", "Cons", "Nil", "succ", "True", "False"]

main :: IO ()
main = do
  fname <- getArgs
  tmp <- readFile (head fname)
  let mod = parseModule tmp
--  pprint $ mod
  putStrLn "-------------------------------------------------------------------------"
  pprint $ convParse $ mod
  ids <- newEnumSupply
  let (ids1, ids2) = split2 ids
  let (Ok (predefBinds,_,_)) = distributeIds predef ids2
  case rename' predefBinds (convParse $ parseModule tmp) ids1 of
    Ok (n,m) -> pprint m >> pprint n >> pprint (invRename n m) >> 
               pprint (invRename n m == (Ok (convParse mod)))
    Hiba f   -> pprint$ "HIBA: " ++ f
               



