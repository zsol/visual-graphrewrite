
import Paths_visual_graphrewrite (version, getDataFileName)

import GraphRewrite
import GraphRewrite.Main.Welcome
import GraphRewrite.Main.CmdLineOpts
import GraphRewrite.Main.Utils


import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Glade as G
import qualified Graphics.Rendering.Cairo.SVG as C

import Language.Haskell.Parser -- parseModule
import IPPrint --pprint
import System.Environment --getArgs
import Data.Supply
import qualified Data.Version

import qualified Data.IntMap as I
import Data.Maybe

import Control.Concurrent (forkIO, yield)
import Control.Arrow hiding (left)
import Control.Monad

import Control.Parallel.Strategies
import Control.Concurrent.MVar.Strict
import System.Directory

import Prelude hiding (exp)

--------------------------------------

data State = State
    { current  :: RewriteTree
    , history  :: Context
    , rewrsys  :: RewriteSystem
    , idsupp   :: Supply Int
    , sWindow  :: G.Window
    , sDrawing :: G.DrawingArea
    , sMap     :: G.DrawingArea
    , dotexe   :: FilePath
    , opts     :: Options
    }


instance NFData State where
    rnf = flip seq ()

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
      let sh = convParse $ parseModuleWithMode (ParseMode (fromMaybe "stdin" $ inputFile options)) tmp
      when (debug options) $ pprint sh
      ids <- newEnumSupply
      let (deltaIds: ids1: ids2: ids3:_) = split ids
      let (Ok (predefBinds,_,_)) = distributeIds predef deltaIds        -- distribute delta function ids
      case rename' predefBinds sh ids2 of -- scope analysis
        Error f   -> error $ "Error: " ++ f
        Ok (names,module_) -> do
           let rs = makeRewriteSystem module_ names
           when (debug options) $ pprint rs

           let rewritetree = case mainTerm options of
                               Nothing -> startRuleDefs rs
                               Just tm -> startRewriting tm rs ids3

           mstate <- newState options rs ids1 rewritetree

           runGUI mstate
--           configureWindow basicKeyBindings mstate msession

cleanup :: MVar State -> IO ()
cleanup mstate = do
  takeMVar mstate
  G.mainQuit

configureBindings :: G.GladeXML -> MVar State -> IO ()
configureBindings xml mstate = do
  window <- G.xmlGetWidget xml G.castToWindow "visual-graphrewrite"

  quitbutton <- G.xmlGetWidget xml G.castToButton "quitbutton"
  leftbutton <- G.xmlGetWidget xml G.castToButton "leftbutton"
  rightbutton <- G.xmlGetWidget xml G.castToButton "rightbutton"
  upbutton <- G.xmlGetWidget xml G.castToButton "upbutton"
  downbutton <- G.xmlGetWidget xml G.castToButton "downbutton"

  G.onClicked quitbutton $ G.widgetDestroy window

  G.onClicked leftbutton $ moveLeft mstate >> refresh mstate >> return ()
  G.onClicked rightbutton $ moveRight mstate >> refresh mstate >> return ()
  G.onClicked upbutton $ moveUp mstate >> refresh mstate >> return ()
  G.onClicked downbutton $ moveDown mstate >> refresh mstate >> return ()

  return ()


runGUI :: MVar State -> IO ()
runGUI mstate = do
  G.initGUI
  uipath <- getDataFileName "ui.glade"
  Just xml <- G.xmlNew uipath
  window <- G.xmlGetWidget xml G.castToWindow "visual-graphrewrite"
  G.onDestroy window (cleanup mstate)

  canvas <- G.xmlGetWidget xml G.castToDrawingArea "drawingarea"
  maparea <- G.xmlGetWidget xml G.castToDrawingArea "map"

  modifyMVar_ mstate $ \ state ->
      return state { sWindow = window, sDrawing = canvas, sMap = maparea }

  configureBindings xml mstate

  G.widgetShowAll window
  G.timeoutAddFull (yield >> return True) G.priorityDefaultIdle 50
  forkIO G.mainGUI

  svg <- C.svgNewFromString welcome
  updateCanvas svg canvas

  G.onExposeRect canvas (const $ refresh mstate >> return ())
  G.onExposeRect maparea (const $ refresh mstate >> return ())

  return ()

startRewriting :: String -> RewriteSystem -> Supply Int -> RewriteTree
startRewriting term rs ids = tree
    where
      tree = rewriteStepFine rs expr gra
      gra = I.fromList []
      binds = namesToBinds (names rs)
      sh = rename' binds (convParse $ parseModuleWithMode (ParseMode "CmdLine Argument") ("tHiSiSaVeRyBaDaNDuGLyHaCK = " ++ term)) ids
      expr = case sh of
               Ok (_, m) -> exp $ head $ I.findMin $ rules $ makeRewriteRules m
               Error err -> error $ "Error during parsing CmdLine argument: " ++ err


{-case I.toList (I.filter (== term) (names rs)) of
                       [(tid, _)] -> case I.lookup tid (rules rs) of
                                      Just [r] -> (exp r, graph r)
                                      Just _ -> error $ "Ambiguous reference to term: " ++ term ++ ". Make sure there is only one rule associated with it in the source file. (Maybe you want to examine another term?)"
                                      Nothing -> error $ "No such term `" ++ term ++ "' found in the source file, it is probably defined elsewhere. (Did you want to examine a delta function?)"
                       [] -> error $ "Absolutely nothing found about this term: " ++ term ++ ". You probably have a typo somewhere."
                       _  -> error $ "Ambiguous reference to term: " ++ term ++ ". This shouldn't happen if your source file compiles with a Haskell compiler."
-}

startRuleDefs :: RewriteSystem -> RewriteTree
startRuleDefs rs = tree
    where
      tree = Step start pgtrees
      start = (SLit "Move down to see rule definitions", I.fromList [])
      pgs = map (exp &&& graph) $ concatMap snd $ I.toList $ rules rs
      pgtrees = map (\pg -> Step pg []) pgs


moveDown :: MVar State -> IO ()
moveDown = flip modifyMVar_ $ \ state ->
             case current state of
               Step _ [] -> return state
               Step g (h:t) -> return state { current = h
                                           , history = RewriteBranch g [] t : history state
                                           }


moveUp :: MVar State -> IO ()
moveUp = flip modifyMVar_ $ \ state ->
           case history state of
             [] -> return state
             (h:t) -> return state { current = Step (node h) (left h ++ [current state] ++ rght h)
                                  , history = t
                                  }

moveRight :: MVar State -> IO ()
moveRight = flip modifyMVar_ $ \ state ->
              case history state of
                []                            -> return state
                (RewriteBranch _ _ []:_)      -> return state
                (RewriteBranch n l (rh:rt):t) ->
                    return state { current = rh
                                 , history = RewriteBranch n (current state : l) rt : t
                                 }

moveLeft :: MVar State -> IO ()
moveLeft = flip modifyMVar_ $ \ state ->
             case history state of
               [] -> return state
               (RewriteBranch _ [] _ : _) -> return state
               (RewriteBranch n (lh:lt) r : t) ->
                   return state { current = lh
                                , history = RewriteBranch n lt (current state : r) : t
                                }

refresh :: MVar State -> IO Bool
refresh mstate = do
  state <- takeMVar mstate
  let (ids, ids') = split2 (idsupp state)
  putMVar mstate (state { idsupp = ids' })

  let (Step pg _) = current state

  when (debug $ opts state) (putStr "DEBUG: " >> pprint pg)

  updateCanvasTo (sDrawing state) pg (dotexe state) (rewrsys state) ids

  dg <- stateToDot (current state) (history state)

  when (debug $ opts state) (putStrLn $ "DEBUG: " ++ show dg)

  svgmap <- dotToSVG (dotexe state) (show dg)
  updateCanvas svgmap (sMap state)

newState :: Options -> RewriteSystem -> Supply Int -> RewriteTree -> IO (MVar State)
newState opts rs ids tree = do
  mdot <- findExecutable "dot"
  let exe = case mdot of
              Nothing -> error "dot binary not found. Please install graphviz"
              Just p -> p



  let s = State { history = []
                , current = tree
                , rewrsys = rs
                , idsupp = ids
                , opts = opts
                , dotexe = exe
                }
  newMVar s
