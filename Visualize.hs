module Visualize
where

--import qualified SimpleHaskell as SH
import RewriteTypes

import Text.PrettyPrint

import Control.Applicative

import Data.Supply

--import Data.Graph.Inductive.Graphviz
import Data.Graph.Inductive.Tree
import qualified Data.Graph.Inductive.Graph as IG
import Data.IntMap hiding (map)
import Data.Maybe
import Data.List (findIndex)
import Prelude hiding (lookup)

type VisGraph = [(String, [String])]

gStyle :: String
gStyle = unlines
         [
          "node [fontcolor=\"#1f33b3\", fontsize=12, shape=none, fontname=\"Helvetica\"];"
         ,"edge [color=\"#000000\", style=dotted, fontname=\"Helvetica\", arrowhead=normal, arrowsize=0.3];"
         ]


ppGraph :: VisGraph -> Doc
ppGraph xs = text "digraph g" <+>
             (braces $
                    text gStyle
                      $+$ nest indent (vcat . fmap ppEdge $ xs))
    where
      indent = 4


ppEdge :: (String, [String]) -> Doc
ppEdge (x, xs) =
    (i x) <+> (brackets $ text "label=" <> n x)
    $$
    vcat (map (((i x) <+> (text "->") <+>) . dQText) xs)
        where
          f = fromMaybe . const (-1) <*> findIndex (== '|')
          n = dQText . ((+1) . f >>= drop)
          i = dQText . (takeWhile (/= '|'))
--    (dQText x) <+> (text "->")
--                   <+> (braces . hcat . punctuate comma . fmap dQText $ xs)

dQText :: String -> Doc
dQText = doubleQuotes . text
{-
exprToGraph :: RewriteSystem -> Expr -> VisGraph
exprToGraph = helper (0 :: Int)
    where
      helper i rs e@(SApp f es) = (show i ++ genLabel rs f, map (\e -> show (i+1) ++ exprID e) es) : concatMap (helper (i+1) rs) es
      helper i rs e = [(show i ++ genLabel rs e, [])]
-}
genLabel :: RewriteSystem -> Expr -> String
genLabel rs (SCons c) = show c ++ "|" ++ lkp rs c
genLabel rs (SFun _ f) = show f ++ "|" ++ lkp rs f
genLabel _ (SLit l) = l
genLabel rs (SHole h) = show h ++ "|" ++ lkp rs h
genLabel rs (SRef r) = show r ++ "|" ++ lkp rs r
genLabel rs (SApp e es)  = unwords $ (genLabel rs e) : map (genLabel rs) es

lkp :: RewriteSystem -> Int -> [Char]
lkp rs i = fromMaybe "UNDEFINED" $ lookup i (names rs)

genName :: RewriteSystem -> Expr -> String
genName rs (SCons c) = lkp rs c
genName rs (SFun _ f) = lkp rs f
genName rs (SHole h) = lkp rs h
genName rs (SRef r) = lkp rs r
genName rs (SApp e es) = unwords $ (genName rs e) : map (genName rs) es
genName _ (SLit l) = l

genID :: Supply Int -> Expr -> Int
genID _ (SCons c) = c
genID _ (SFun _ f) = f
genID _ (SHole h) = h
--genID _ (SRef r) = r
genID ids _ = supplyValue ids


genLNode :: Supply Int -> RewriteSystem -> Expr -> IG.LNode String
genLNode ids rs e = (genID ids e, genName rs e)

genLEdge :: Supply Int -> Expr -> Expr -> IG.LEdge String
genLEdge ids e f = (genID i e, genID j f, "")
    where
      (i, j) = split2 ids

graphToGr :: Supply Int -> RewriteSystem -> PointedGraph -> Gr String String
graphToGr ids rs (e, g) = insExpr (-1) ids e IG.empty
    where
      insExpr i ids e@(SRef r) gr = let
          refe = fromJust (lookup r g)
          (ids1, ids2) = split2 ids
        in
          if i < 0 then
              let en@(eid, _) = genLNode ids1 rs e in
              IG.insEdge (eid, r, "") $ insExpr r ids2 refe $ IG.insNode en gr
          else
              if IG.gelem i gr then
                  gr
              else
                  IG.insEdge (i, r, "") $ insExpr r ids2 refe $ IG.insNode (i, genName rs e) gr
      insExpr i ids e gr
          | i < 0 = IG.insNode (genLNode ids rs e) gr
          | otherwise = IG.insNode (i, genName rs e) gr

outputDot :: Gr String String -> String -> IO ()
outputDot gr fname = writeFile fname $ render $ ppGr gr --graphviz gr "g" (5,5) (1,1) Portrait

ppGr :: Gr String String -> Doc
ppGr gr = text "digraph g" <+>
          (braces $
                  text gStyle $+$
                  vcat pnodes $+$
                  vcat pedges)
    where
--      f = uncurry (liftM2 (,))
--      (nodes, edges) = (f (labNodes, labEdges)) gr
      (nodes, edges) = (IG.labNodes gr, IG.labEdges gr)
      pnodes = map (\(x,y) -> dQText (show x) <+> (brackets $ text "label =" <+> dQText y)) nodes
      pedges = map (\(x,y,_) -> dQText (show x) <+> text "->" <+> dQText (show y)) edges

{-

insNode (2 SRef 1 $ insNode (1, "b") $ insNode (4, "a") empty

-}
