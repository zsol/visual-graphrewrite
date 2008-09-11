import System.Environment --getArgs
import Language.Haskell.Parser -- parseModule
import IPPrint --pprint
--import DataGraph
import Language.Haskell.Syntax
import Rename

main :: IO ()
main = do
  fname <- getArgs
  tmp <- readFile (head fname)
  pprint (parseModule tmp)



