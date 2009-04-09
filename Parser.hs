
import Paths_visual_graphrewrite (version)

import Rename
import Convert
import Rewrite
import RewriteApp
import RewriteAppTypes
--import DataGraph
import CmdLineOpts

import Language.Haskell.Parser -- parseModule
import IPPrint --pprint
import Language.Haskell.Syntax
import System.Environment --getArgs
import qualified Data.Version
import Data.Supply

import Data.Map
import Data.Maybe ( fromMaybe )


predef :: [String]
predef = ["++", "div", "mod", "eqInt", "not", "Cons", "Nil", "succ", "True", "False"]

main :: IO ()
main = do
  args <- getArgs
  options <- parseOptions args
  if showVersion options
    then putStrLn $ "version " ++ Data.Version.showVersion version
    else do
      tmp <- readInput (inputFile options)
      let mod = parseModule tmp
    --pprint $ mod
    --putStrLn "-------------------------------------------------------------------------"
      pprint $ convParse $ mod
      ids <- newEnumSupply
      let (ids1, ids2) = split2 ids
      let (Ok (predefBinds,_,_)) = distributeIds predef ids2
      let (Ok (n,m)) = rename' predefBinds (convParse $parseModule tmp) ids1
      let rs = makeRewriteRules m
      pprint rs
{-      case rename' predefBinds (convParse $ parseModule tmp) ids1 of
        Ok (n,m) -> pprint m >> pprint n >> pprint (invRename n m) >>
                   pprint (makeRewriteRules m)
        Hiba f   -> pprint$ "HIBA: " ++ f
-}


readInput :: Maybe String -> IO String
readInput Nothing = getContents
readInput (Just f) = readFile f


