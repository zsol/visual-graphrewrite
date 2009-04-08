
import Paths_visual_graphrewrite (version)

import Rename
import Convert
import Rewrite
import RewriteApp
import RewriteAppTypes
--import DataGraph

import Language.Haskell.Parser -- parseModule
import IPPrint --pprint
import Language.Haskell.Syntax
import System.Environment --getArgs
import System.Console.GetOpt
import qualified Data.Version
import Data.Supply

import Data.Map
import Data.Maybe ( fromMaybe )

----------------------------------- options record

data Options = Options
    { showVersion   :: Bool             -- ^ just show the version and exit
    , inputFile     :: Maybe String     -- ^ Nothing: stdin
    , outputFile    :: Maybe String     -- ^ Nothing: stdout
    , isVerbose     :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { showVersion   = False
    , inputFile     = Nothing
    , outputFile    = Nothing
    , isVerbose     = False
    }

----------------------------------- possible options

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['v']     ["verbose"] (NoArg (\r -> r {isVerbose   = True}))       "verbose output on stderr"
    , Option ['V','?'] ["version"] (NoArg (\r -> r {showVersion = True}))       "show version number"
    , Option ['o']     ["output"]  (ReqArg (\f r -> r {outputFile = Just f}) "FILE")  "output FILE"
    , Option ['c']     []          (ReqArg (\f r -> r {inputFile  = Just f}) "FILE")  "input FILE"
    ]

----------------------------------- parse options

addInput :: String -> Options -> Options
addInput f r = r {inputFile = Just f}

parseOptions :: [String] -> IO Options
parseOptions argv = do
    progName <- getProgName   
    let header = "Usage: " ++ progName ++ " [OPTION...] files..."
    case getOpt Permute options argv of
      (o,n,[]  ) -> let
                        o'  = foldl (flip id) defaultOptions o
                        o'' = foldl (flip addInput) o' n
                    in return o''
      (_,_,errs) -> error (concat errs ++ usageInfo header options)

{-
id              :: (a -> b) -> a -> b
flip id         :: a -> (a -> b) -> b
foldl (flip id) :: b -> [b -> b] -> b

  foldl (flip id) 3 [(+1),(*2)] == 8
-}

-----------------------------------

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
               

readInput :: Maybe String -> IO String
readInput Nothing = getContents
readInput (Just f) = readFile f


