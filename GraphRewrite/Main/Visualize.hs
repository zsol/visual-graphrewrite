module GraphRewrite.Main.Visualize
where

-- TODO: use the graphviz package

import GraphRewrite.Internal.DeltaFunctions
import GraphRewrite.Internal.RewriteTypes

import Text.PrettyPrint
import Data.Supply
import Data.Graph.Inductive.Tree
import qualified Data.Graph.Inductive.Graph as IG
import Data.IntMap hiding (map)
import Data.Maybe
import Prelude hiding (lookup, exp)


gStyle :: String
gStyle = unlines
         [
          "node [fontcolor=\"#1f33b3\", fontsize=12, shape=box, fontname=\"Helvetica\"];"
         ,"edge [color=\"#000000\", style=dotted, fontname=\"Helvetica\", arrowhead=normal, arrowsize=0.3];"
         ]


dQText :: String -> Doc
dQText = doubleQuotes . text . concatMap quoteChar
 where
    quoteChar :: Char -> String
    quoteChar '\"' = "\\\""
    quoteChar c = [c]

lookupName :: RewriteSystem -> Int -> [Char]
lookupName rs i = fromMaybe "UNDEFINED" $ lookup i (names rs)

-- | RewriteSystem needed for identifier names.
genName :: RewriteSystem -> Expr -> String
genName rs = gName (-1)  where

    gName :: Int{-outerPrec-} -> Expr -> String
    gName _ (SCons c) = lookupName rs c
    gName _ (SFun _ f) = lookupName rs f
    gName _ (SHole h) = lookupName rs h
    gName _ (SRef r) = lookupName rs r
    gName _ (SLit l) = l
    gName p e@(SApp _ _) = rearrange ip $ fn : map (gName $ precedence ip) xs  where

      precedence :: Prec -> Int
      precedence (Infixr i) = i
      precedence Prefix = 10

      rearrange Prefix l = addParents (p>9) $ unwords l
      rearrange _ [a,b,c] = addParents (p>9) $ unwords [b,a,c]
      rearrange _ [a,b] = addParents True $ unwords [b,a]
      rearrange _ [a] = addParents True a

      ip = head $ [ip | (ip, n)<-deltaNames, n == fn] ++ [Prefix]
      fn = gName 10 f

      (f, xs) = flattenSApp e

      flattenSApp :: Expr -> (Expr, [Expr])
      flattenSApp (SApp y ys) = (f, xs ++ ys)    where (f, xs) = flattenSApp y
      flattenSApp x = (x, [])

addParents :: Bool -> String -> String
addParents True s = "(" ++ s ++ ")"
addParents False s = s



genID :: Supply Int -> Expr -> Int
genID _ (SCons c) = c
genID _ (SFun _ f) = f
genID _ (SHole h) = h
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
          refe = case lookup r g of
                   Just r -> r
                   _ -> case lookup r (rules rs) of
                         Just [r] -> exp r
                         _ -> error $ "Undefined reference: " ++ show r ++ ". Check your source."
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
