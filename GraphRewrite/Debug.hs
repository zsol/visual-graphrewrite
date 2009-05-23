
import GraphRewrite.Internal.SimpleHaskell
import IPPrint
import Language.Haskell.Syntax
import Language.Haskell.Parser
import GraphRewrite.Internal.Rename
import GraphRewrite.Internal.Convert
import Data.Supply
import qualified Data.IntMap as I
import Data.Map
import GraphRewrite.Internal.RewriteApp
import GraphRewrite.Internal.RewriteTypes
import GraphRewrite.Internal.Rewrite
import GraphRewrite.Internal.DeltaFunctions
import GraphRewrite.Main.Visualize
import Prelude hiding (exp)
import Foreign
import Data.Maybe
import Data.Graph.Inductive.Graphviz

predef = Prelude.map snd deltaNames

sup = unsafePerformIO newEnumSupply :: Supply Int
file = unsafePerformIO $ readFile "sample/SieveOfEratosthenes.hs"
(ids, ids2, ids3) = split3 sup

(Ok (predefBinds,_,_)) = distributeIds predef ids2

m = parseModule file
renamed = rename' predefBinds (convParse m) ids3
(Ok (n,sm)) = renamed
rs = makeRewriteSystem sm n

-------

(i1, i2, i3) = split3 ids

(Ok (nev, modul)) = rename' predefBinds (convParse $ parseModule "result = f 1; f 5 = 5; f a = f (succ a);") i1
rendsz =  makeRewriteSystem modul nev
pg = let r = (head $ fromJust $ I.lookup 0 (rules rendsz)) in (exp r, graph r)
