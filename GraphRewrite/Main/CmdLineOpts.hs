module GraphRewrite.Main.CmdLineOpts
where

import System.Console.GetOpt
import System.Environment

data Options = Options
    { showVersion   :: Bool             -- ^ just show the version and exit
    , inputFile     :: Maybe String     -- ^ Nothing: stdin
    , outputFile    :: Maybe String     -- ^ Nothing: stdout
    , isVerbose     :: Bool
    , mainTerm      :: Maybe String     -- ^ Name of the main term on which to perform rewriting (example: result)
    , stepNum       :: Int              -- ^ How many rewrite steps should we show at most
    }

defaultOptions :: Options
defaultOptions = Options
    { showVersion   = False
    , inputFile     = Nothing
    , outputFile    = Nothing
    , isVerbose     = False
    , mainTerm      = Nothing
    , stepNum       = 50
    }

----------------------------------- possible options

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['v']     ["verbose"] (NoArg  (\r -> r {isVerbose   = True}))       "verbose output on stderr"
    , Option ['V','?'] ["version"] (NoArg  (\r -> r {showVersion = True}))       "show version number"
    , Option ['o']     ["output"]  (ReqArg (\f r -> r {outputFile = Just f}) "FILE")  "output FILE"
    , Option ['i']     ["input"]   (ReqArg (\f r -> r {inputFile  = Just f}) "FILE")  "input FILE"
    , Option ['m']     ["main"]    (ReqArg (\m r -> r {mainTerm = Just m}) "TERM") "main term on which to perform rewriting"
    , Option ['n']     ["stepnum"] (ReqArg (\n r -> r {stepNum = read n}) "NUM") "how many rewrite steps should we show at most"
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
