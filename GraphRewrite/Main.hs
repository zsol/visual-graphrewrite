
import Paths_visual_graphrewrite (version)

import GraphRewrite
import GraphRewrite.Main.Welcome
import GraphRewrite.Main.CmdLineOpts
import GraphRewrite.Main.Utils


import qualified Graphics.UI.Gtk as G
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
    { current :: RewriteTree
    , history :: Context
    , rewrsys :: RewriteSystem
    , idsupp  :: Supply Int
    }

data Session = Session {
      sWindow :: !G.Window,
      sDrawing :: !G.DrawingArea,
      dotexe :: !FilePath,
      opts :: !Options
}

instance NFData Session where
    rnf x = x `seq` ()

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
      let (deltaIds, ids2, ids3) = split3 ids
      let (Ok (predefBinds,_,_)) = distributeIds predef deltaIds        -- distribute delta function ids
      case rename' predefBinds sh ids2 of -- scope analysis
        Error f   -> error $ "Error: " ++ f
        Ok (names,module_) -> do
           let rs = makeRewriteSystem module_ names
           msession <- newSession options
           when (debug options) $ pprint rs
           mstate <- case mainTerm options of
             Nothing -> startRuleDefs rs ids3
             Just tm -> startRewriting tm rs ids3
           configureWindow basicKeyBindings mstate msession


startRewriting :: String -> RewriteSystem -> Supply Int -> IO (MVar State)
startRewriting term rs ids = newState rs ids' tree
    where
      tree = rewriteStepFine rs expr gra
      gra = I.fromList []
      (ids', ids'') = split2 ids
      binds = namesToBinds (names rs)
      sh = rename' binds (convParse $ parseModuleWithMode (ParseMode "CmdLine Argument") ("tHiSiSaVeRyBaDaNDuGLyHaCK = " ++ term)) ids''
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

startRuleDefs :: RewriteSystem -> Supply Int -> IO (MVar State)
startRuleDefs rs ids = newState rs ids tree
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

refresh :: MVar State -> MVar Session -> IO Bool
refresh mstate msess = do
  session <- readMVar msess

  state <- takeMVar mstate
  let (ids, ids') = split2 (idsupp state)
  putMVar mstate (state { idsupp = ids' })

  let (Step pg _) = current state

  when (debug $ opts session) (putStr "DEBUG: " >> pprint pg)

  updateCanvasTo (sDrawing session) pg (dotexe session) (rewrsys state) ids


basicKeyBindings :: (G.WidgetClass w) => AssocList String (w -> MVar State -> MVar Session -> IO ())
basicKeyBindings = [("q", \w _ _ -> G.widgetDestroy w)
                   ,("space", \_ a b -> moveRight a >> refresh a b >> return ())
                   ,("BackSpace", \_ a b -> moveLeft a >> refresh a b >> return ())
                   ,("d", \_ a b -> moveDown a >> refresh a b >> return ())
                   ,("u", \_ a b -> moveUp a >> refresh a b >> return ())
                   ,("r", \_ a b -> moveRight a >> refresh a b >> return ())
                   ,("l", \_ a b -> moveLeft a >> refresh a b >> return ())
                   ]

newState :: RewriteSystem -> Supply Int -> RewriteTree -> IO (MVar State)
newState rs ids tree = do
  let s = State { history = []
                , current = tree
                , rewrsys = rs
                , idsupp = ids
                }
  newMVar s

newSession :: Options -> IO (MVar Session)
newSession opts = do
  --G.unsafeInitGUIForThreadedRTS
  G.initGUI
  window <- G.windowNew
  canvas <- G.drawingAreaNew
  svg <- C.svgNewFromString welcome

  G.set window [G.containerChild G.:= canvas]
  G.windowSetDefaultSize window 400 400
  G.widgetShowAll window
  G.timeoutAddFull (yield >> return True) G.priorityDefaultIdle 50 -- magic, do not touch
  forkIO G.mainGUI

  mdot <- findExecutable "dot"
  let exe = case mdot of
              Nothing -> error "dot binary not found. Please install graphviz"
              Just p -> p

  let s = Session {
               sWindow = window,
               sDrawing = canvas,
               dotexe = exe,
               opts = opts
             }

  updateCanvas svg canvas

  newMVar $! s

configureWindow
    :: AssocList String (G.Window -> MVar State -> MVar Session -> IO ())
    -> MVar State
    -> MVar Session
    -> IO ()
configureWindow keyBindings mstate msess = do
  session <- readMVar msess
  let window = sWindow session
  let canvas = sDrawing session
  G.onKeyPress window $ handleKeys keyBindings window mstate msess
  G.onDestroy window (takeMVar mstate >> takeMVar msess >> G.mainQuit)
  G.onExposeRect canvas (const $ refresh mstate msess >> return ())
  return ()

