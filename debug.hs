
import SimpleHaskell
import IPPrint
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Rename
import Convert
import Data.Supply
import Data.Map
import RewriteApp
import RewriteAppTypes

import Foreign

predef = ["++", "div", "mod", "eqInt", "not", "Cons", "Nil", "succ", "True", "False"]

sup = unsafePerformIO newEnumSupply :: Supply Int
file = unsafePerformIO $ readFile "sample/eratosthenes.hs"
(ids, ids2, ids3) = split3 sup

(Ok (predefBinds,_,_)) = distributeIds predef ids2

m = parseModule file
renamed = rename' predefBinds (convParse m) ids3
(Ok (_,sm)) = renamed