
import Paths_visual_graphrewrite (version)

import GraphRewrite
import GraphRewrite.Main.Welcome
import GraphRewrite.Main.CmdLineOpts
import GraphRewrite.Main.Visualize


import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as G
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.SVG as C

import Language.Haskell.Parser -- parseModule
import IPPrint --pprint
import System.Environment --getArgs
import Data.Supply
import qualified Data.Version

import qualified Data.IntMap as I

import System.Process
import Control.Concurrent (forkIO, yield)
import System.IO
import Control.Arrow
import Control.Monad
import System.Exit
import qualified Control.Exception as C

import Control.Parallel.Strategies
import System.IO.Unsafe
import Control.Concurrent.MVar.Strict
import System.Directory

import Prelude hiding (exp)

--------------------------------------

data Session = Session {
      sWindow :: !G.Window,
      sDrawing :: !G.DrawingArea,
      sSVGs :: ![C.SVG],
      cur :: !Int
}

instance NFData Session where
    rnf x = x `seq` ()

predef :: [String]
predef = map snd deltaNames

main :: IO ()
main = do
  args <- getArgs
  options <- parseOptions args
  if showVersion options
    then putStrLn $ "version " ++ Data.Version.showVersion version
    else do
      tmp <- readInput (inputFile options)
      let mod = parseModule tmp
      when (debug options) $ pprint $ convParse mod
      ids <- newEnumSupply
      let (deltaIds, ids2, ids3) = split3 ids
      let (Ok (predefBinds,_,_)) = distributeIds predef deltaIds        -- distribute delta function ids
      case rename' predefBinds (convParse $parseModule tmp) ids2 of -- scope analysis
        Error f   -> error $ "Error: " ++ f
        Ok (names,module_) -> do
           let rs = makeRewriteSystem module_ names
           when (debug options) $ pprint rs
           G.timeoutAddFull (yield >> return True) G.priorityDefaultIdle 50 -- magic, do not touch
           case mainTerm options of
             Nothing -> addRuleDefs rs ids3
             Just tm -> startRewriting tm (stepNum options) rs ids3

startRewriting :: String -> Int -> RewriteSystem -> Supply Int -> IO ()
startRewriting term n rs ids = case I.toList (I.filter (== term) (names rs)) of
    [(tid, _)] -> case I.lookup tid (rules rs) of
                   Just [r] -> do
                     addToSession (renderDot ids1 rs (exp r, graph r))
                     mapM_ (addToSession . (\(x,y) -> renderDot x rs y)) (zip (split ids2) (take n $ rewriteSteps rs (exp r) (graph r)))

                   Just _ -> error $ "Ambiguous reference to term: " ++ term ++ ". Make sure there is only one rule associated with it in the source file. (Don't you want to examine another term?)"
                   Nothing -> error $ "No such term `" ++ term ++ "' found in the source file, it is probably defined elsewhere. (Did you want to examine a delta function?)"
    [] -> error $ "Absolutely nothing found about this term: " ++ term ++ ". You probably have a typo somewhere."
    _  -> error $ "Ambiguous reference to term: " ++ term ++ ". This shouldn't happen if your source file compiles with a Haskell compiler."
    where
      (ids1, ids2) = split2 ids


addRuleDefs :: RewriteSystem -> Supply Int -> IO ()
addRuleDefs rs ids = mapM_ addToSession grs
    where
      grs = map (\(x,y) -> renderDot x rs y) (zip (split ids) pgs)
      pgs = map (exp &&& graph) $ concatMap snd $ I.toList $ rules rs

readInput :: Maybe String -> IO String
readInput Nothing = getContents
readInput (Just f) = readFile f

nextSVG :: Session -> Session
nextSVG s@(Session _ _ svgs i)
    | i+1 >= length svgs = s { cur = 1 }
    | otherwise          = s { cur = i+1 }

prevSVG :: Session -> Session
prevSVG s@(Session _ _ svgs i)
    | i <= 1    = s { cur = length svgs - 1 }
    | otherwise = s { cur = i - 1 }

addToSession :: String -> IO Int
addToSession dot = do
  noSession <- isEmptyMVar sessionRef
  () <- when noSession newSession

  mdot <- findExecutable "dot"
  let exe = case mdot of
              Nothing -> error "dot binary not found. Please install graphviz"
              Just p  -> p

  svgstring <- myReadProcess exe ["-Tsvg"] dot
  svg       <- C.svgNewFromString svgstring

  modifyMVar sessionRef $ \(Session w c svgs i) -> return ((Session w c (svgs ++ [svg]) i), length svgs)


updateCanvas :: C.SVG -> G.DrawingArea -> IO Bool
updateCanvas svg canvas = do
  win <- G.widgetGetDrawWindow canvas
  (width, height) <- G.widgetGetSize canvas
  let (w,h)    = (fromIntegral width, fromIntegral height)
      (sw, sh) = C.svgGetSize svg

  G.renderWithDrawable win $ do
                            C.setAntialias C.AntialiasDefault
                            C.setLineCap C.LineCapSquare

                            C.scale (w / fromIntegral sw) (h / fromIntegral sh)
                            C.svgRender svg
  return True

myReadProcess
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout + stderr
myReadProcess cmd args input = do
    (Just inh, Just outh, _, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = Inherit }

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    forkIO $ C.evaluate (length output) >> putMVar outMVar ()

    -- now write and flush any input
    unless (null input) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    case ex of
     ExitSuccess   -> return output
     ExitFailure _ -> return output

type AssocList a b = [(a, b)]

handleKeys :: (Monad m, G.WidgetClass w) => AssocList String (w -> m a) -> w -> G.Event -> m Bool
handleKeys m w (G.Key {G.eventKeyName = key})
    = case lookup key m of
          Just a -> a w >> return True
          _      -> return True

basicKeyBindings :: (G.WidgetClass w) => AssocList String (w -> IO ())
basicKeyBindings =       [("q", G.widgetDestroy)
                         ,("space", const $ do
                             modifyMVar_ sessionRef (return . nextSVG)
                             withMVar sessionRef $ \(Session _ c svgs cur) ->
                                 updateCanvas (svgs !! cur) c
                             return ()
                          )
                         ,("BackSpace", const $ do
                             modifyMVar_ sessionRef (return . prevSVG)
                             withMVar sessionRef $ \(Session _ c svgs cur) ->
                                 updateCanvas (svgs !! cur) c
                             return ()
                          )
                         ]

sessionRef :: MVar Session
sessionRef = unsafePerformIO newEmptyMVar
{-# NOINLINE sessionRef #-}

newSession :: IO ()
newSession = newSessionWith basicKeyBindings

--does not compile with this:
--             :: (G.WidgetClass w) => D.Map String (w -> IO ()) -> IO ()
newSessionWith :: AssocList String (G.Window -> IO ()) -> IO ()
newSessionWith keyBindings = do
  G.unsafeInitGUIForThreadedRTS
  window <- G.windowNew
  canvas <- G.drawingAreaNew
  svg <- C.svgNewFromString welcome

  G.onKeyPress window $ handleKeys keyBindings window
  G.onDestroy  window (takeMVar sessionRef >> G.mainQuit)

  G.onExposeRect canvas $ const $ do
               withMVar sessionRef $ \(Session _ c svgs cur) ->
                   updateCanvas (svgs !! cur) c
               return ()

  G.set window [G.containerChild G.:= canvas]
  G.windowSetDefaultSize window 400 400
  G.widgetShowAll window
  forkIO G.mainGUI

  let s = Session {
               sWindow = window,
               sDrawing = canvas,
               sSVGs = [svg],
               cur = 0
             }

  putMVar sessionRef $! s

