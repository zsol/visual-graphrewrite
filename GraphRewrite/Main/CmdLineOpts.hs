module GraphRewrite.Main.CmdLineOpts
where

import System.Console.GetOpt
import System.Environment

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
    , Option ['i']     ["input"]   (ReqArg (\f r -> r {inputFile  = Just f}) "FILE")  "input FILE"
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
