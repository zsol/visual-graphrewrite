
--import Paths_visual_graphrewrite (version)

import Visualize
import Rename
import Convert
import Rewrite
import RewriteApp
import RewriteTypes
--import DataGraph
import CmdLineOpts
import DeltaFunctions

import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as G
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.SVG as C
import qualified Graphics.UI.Gtk.Gdk.DrawWindow as G
import qualified Graphics.UI.Gtk.Gdk.Gdk as G

import Text.PrettyPrint
import Data.Graph.Inductive

import Language.Haskell.Parser -- parseModule
import IPPrint --pprint
import Language.Haskell.Syntax
import System.Environment --getArgs
import qualified Data.Version
import Data.Supply

import qualified Data.Map as D
import qualified Data.List as D
import Data.Maybe ( fromMaybe )

import System.Process
import Control.Concurrent (forkIO, yield)
import System.IO
import Control.Monad
import System.Exit
import qualified Control.Exception as C

import Control.Parallel.Strategies
import System.IO.Unsafe
import Control.Concurrent.MVar.Strict
import System.Directory

data Session = Session {
      sWindow :: !G.Window,
      sDrawing :: !G.DrawingArea,
      sSVG :: !C.SVG
}

instance NFData Session where
    rnf x = x `seq` ()

predef :: [String]
predef = deltaNames

main :: IO ()
main = do
  args <- getArgs
  options <- parseOptions args
  if showVersion options
    then putStrLn $ "version " -- ++ Data.Version.showVersion version
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
      let rs = makeRewriteSystem m n
      pprint rs
{-      case rename' predefBinds (convParse $ parseModule tmp) ids1 of
        Ok (n,m) -> pprint m >> pprint n >> pprint (invRename n m) >>
                   pprint (makeRewriteRules m)
        Hiba f   -> pprint$ "HIBA: " ++ f
-}


readInput :: Maybe String -> IO String
readInput Nothing = getContents
readInput (Just f) = readFile f

view :: Gr String String -> IO ()
view gr = do
  noSession <- isEmptyMVar sessionRef
  () <- when noSession $ newSession

  let dot = render $ ppGr gr

  mdot <- findExecutable "dot"
  let exe = case mdot of
              Nothing -> error "dot binary not found. Please install graphviz"
              Just p  -> p

  svgstring <- myReadProcess exe ["-Tsvg"] dot
  svg       <- C.svgNewFromString svgstring
  writeFile "/tmp/vis-tmp.svg" svgstring

  c <- modifyMVar sessionRef $ \(Session win canvas svg') -> do
            updateCanvas svg canvas
            return ((Session win canvas svg), canvas)

  G.widgetQueueDraw c
  yield
  return ()

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
    when (not (null input)) $ do hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    case ex of
     ExitSuccess   -> return output
     ExitFailure r -> return output

anyKey :: (Monad m) => m a -> G.Event -> m Bool
anyKey m (G.Key {G.eventKeyName = key})
    | any (`D.isPrefixOf` key) ignores = return True
    | otherwise                      = m >> return True
    where
      ignores = ["Shift", "Control", "Alt", "Super", "Meta", "Hyper"]

sessionRef = unsafePerformIO $ newEmptyMVar
{-# NOINLINE sessionRef #-}

newSession :: IO ()
newSession = do
  G.unsafeInitGUIForThreadedRTS
  window <- G.windowNew
  canvas <- G.drawingAreaNew
  svg <- C.svgNewFromString ""

  G.onKeyPress window $ anyKey (G.widgetDestroy window)
  G.onDestroy  window (takeMVar sessionRef >> G.mainQuit)

  G.onExposeRect canvas $ const $ do
               withMVar sessionRef $ \(Session _ c svg) ->
                   updateCanvas svg c
               return ()

  G.onExposeRect window $ const $ do
               withMVar sessionRef $ \(Session _ c svg) ->
                   updateCanvas svg c
               return ()

  G.set window [G.containerChild G.:= canvas]
  G.windowSetDefaultSize window 400 400
  G.widgetShowAll window
  forkIO G.mainGUI

  let s = Session {
               sWindow = window,
               sDrawing = canvas,
               sSVG = svg
             }

  putMVar sessionRef $! s