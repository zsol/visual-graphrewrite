module GraphRewrite.Main.Utils where

import GraphRewrite
import GraphRewrite.Main.Visualize

import qualified Graphics.UI.Gtk as G hiding (Color)
import qualified Graphics.UI.Gtk.Gdk.Events as G
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.SVG as C

import Data.IsEvaluated
import qualified Data.GraphViz.Types as G
import qualified Data.GraphViz.Attributes as G
import Data.Supply

import Control.Monad
import Control.Concurrent.MVar.Strict
import Control.Concurrent (forkIO)
import qualified Control.Exception as C
import System.Process
import System.IO
import System.Exit

type AssocList a b = [(a, b)]

stateToDot :: RewriteTree -> Context -> IO G.DotGraph
stateToDot tree ctx = do
  (supp', supp'') <- liftM split2 newEnumSupply
  let attrs = []

  treelinks <- getTreeLinks supp' tree
  let treenodeids = map fst treelinks
  let treenodes = map (\x -> G.DotNode x (nodeattrs x (head treenodeids))) treenodeids
  let treeedges = map (\(x,y) -> G.DotEdge x y [] False) (getEdges treelinks)

  contextlinks <- getContextLinks supp'' ctx (fst $ head treelinks)
  let contextnodeids = map fst contextlinks
  let contextnodes = map (\x -> G.DotNode x (nodeattrs 0 1)) contextnodeids
  let contextedges = map (\(x,y) -> G.DotEdge x y [] False) (getEdges contextlinks)

  return $ G.DotGraph attrs (contextnodes ++ treenodes) (contextedges ++ treeedges) False
    where
      nodeattrs :: Int -> Int -> [G.Attribute]
      nodeattrs n m
          | n == m = (G.Color $ G.RGB 0 0 255) : (nodeattrs 0 1)
          | otherwise = [G.Height 0.1, G.Width 0.1, G.FixedSize True, G.Shape G.Circle, G.Style G.Filled, G.Label ""]

getContextLinks :: Supply Int -> Context -> Int -> IO [(Int, [Int])]
getContextLinks n (h:t) prev = do
  let (n1, n2, n3, n4) = split4 n
  leftlinks <- (liftM concat) $ mapM (uncurry getTreeLinks) (zip (split n1) (left h))
  rightlinks <- (liftM concat) $ mapM (uncurry getTreeLinks) (zip (split n2) (rght h))
  let links = leftlinks ++ rightlinks
  let cur = supplyValue n3
  rest <- getContextLinks n4 t cur
  return $ (cur, prev : map fst links) : links ++ rest

getContextLinks _ _ prev = return [(prev, [])]

treeToDot :: RewriteTree -> IO G.DotGraph
treeToDot t = do
  let attrs = []
  supp <- newEnumSupply
  links <- getTreeLinks supp t
  let nodeids = map fst links
  let nodes = map (\x -> G.DotNode x nodeattrs) nodeids
  let edges = map (\(x,y) -> G.DotEdge x y [] False) (getEdges links)
  return $ G.DotGraph attrs nodes edges False
    where
      nodeattrs = [G.Height 0.1, G.Width 0.1, G.FixedSize True, G.Shape G.Circle, G.Style G.Filled, G.Label ""]

getEdges :: [(Int, [Int])] -> [(Int, Int)]
getEdges ((h1, (h2:t1)):t2) = (h1, h2) : getEdges ((h1,t1):t2)
getEdges ((_, []):t2) = getEdges t2
getEdges [] = []

getTreeLinks :: Supply Int -> RewriteTree -> IO [(Int, [Int])]
getTreeLinks n t = do
  evaled <- isEvaluated t
  if evaled then
      case t of
        Step _ ts -> do
                 let (n', n'') = split2 n
                 let ts' = zip (split n') ts
                 moar <- mapM (uncurry getTreeLinks) ts'
                 let moar' = concat moar
                 let moarids = map fst moar'
                 return $ (supplyValue n'', moarids) : moar'
   else
      return [(supplyValue n, [])]

handleKeys :: (Monad m, G.WidgetClass w) => AssocList String (w -> MVar a -> m c) -> w -> MVar a -> G.Event -> m Bool
handleKeys m w ma (G.Key {G.eventKeyName = key})
    = case lookup key m of
          Just a -> a w ma >> return True
          _      -> return True

updateCanvasTo :: G.DrawingArea -> PointedGraph -> FilePath -> RewriteSystem -> Supply Int -> IO Bool
updateCanvasTo canvas pg exe rs ids = do
  let dot = renderDot ids rs pg
  svg <- dotToSVG exe dot
  updateCanvas svg canvas


dotToSVG :: FilePath -> String -> IO C.SVG
dotToSVG dotexe dot = do
  svgstring <- myReadProcess dotexe ["-Tsvg"] dot
  C.svgNewFromString svgstring

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

