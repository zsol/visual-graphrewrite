
import SimpleHaskell
import IPPrint
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Rename
import Convert
import Data.Supply
import qualified Data.IntMap as I
import Data.Map
import RewriteApp
import RewriteTypes
import Rewrite
import DeltaFunctions
import Visualize

import Foreign

predef = deltaNames

sup = unsafePerformIO newEnumSupply :: Supply Int
file = unsafePerformIO $ readFile "sample/eratosthenes.hs"
(ids, ids2, ids3) = split3 sup

(Ok (predefBinds,_,_)) = distributeIds predef ids2

m = parseModule file
renamed = rename' predefBinds (convParse m) ids3
(Ok (n,sm)) = renamed
rs = makeRewriteSystem sm n