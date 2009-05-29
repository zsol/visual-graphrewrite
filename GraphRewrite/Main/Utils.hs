module GraphRewrite.Main.Utils where

import GraphRewrite
import GraphRewrite.Main.Visualize

import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Gdk.Events as G
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.SVG as C

import Data.Supply

import Control.Monad
import Control.Concurrent.MVar.Strict
import Control.Concurrent (forkIO)
import qualified Control.Exception as C
import System.Process
import System.IO
import System.Exit

type AssocList a b = [(a, b)]

handleKeys :: (Monad m, G.WidgetClass w) => AssocList String (w -> MVar a -> MVar b -> m c) -> w -> MVar a -> MVar b -> G.Event -> m Bool
handleKeys m w ma mb (G.Key {G.eventKeyName = key})
    = case lookup key m of
          Just a -> a w ma mb >> return True
          _      -> return True

updateCanvasTo :: G.DrawingArea -> PointedGraph -> FilePath -> RewriteSystem -> Supply Int -> IO Bool
updateCanvasTo canvas pg exe rs ids = do
  let dot = renderDot ids rs pg
  svgstring <- myReadProcess exe ["-Tsvg"] dot
  svg <- C.svgNewFromString svgstring
  updateCanvas svg canvas


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

readInput :: Maybe String -> IO String
readInput Nothing = getContents
readInput (Just f) = readFile f

