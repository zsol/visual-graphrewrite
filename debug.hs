
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
import Prelude hiding (exp)
import Foreign
import Data.Maybe
import Data.Graph.Inductive.Graphviz

predef = deltaNames

sup = unsafePerformIO newEnumSupply :: Supply Int
file = unsafePerformIO $ readFile "sample/eratosthenes.hs"
(ids, ids2, ids3) = split3 sup

(Ok (predefBinds,_,_)) = distributeIds predef ids2

m = parseModule file
renamed = rename' predefBinds (convParse m) ids3
(Ok (n,sm)) = renamed
rs = makeRewriteSystem sm n

-------

(i1, i2, i3) = split3 ids

(Ok (nev, modul)) = rename' predefBinds (convParse $ parseModule "result x = let {a = Cons 1 b; b=a} in a") i1
rendsz =  makeRewriteSystem modul nev
pg = let r = (head $ fromJust $ I.lookup 0 (rules rendsz)) in (exp r, graph r)

gr = graphToGr i2 rendsz pg