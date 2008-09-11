import System.Environment --getArgs
import Language.Haskell.Parser -- parseModule
import IPPrint --pprint
--import DataGraph
import Language.Haskell.Syntax
import Rename
import Data.Supply

main :: IO ()
main = do
  fname <- getArgs
  tmp <- readFile (head fname)
  ids <- newEnumSupply
  pprint $ rename (convParse $ parseModule tmp) ids



