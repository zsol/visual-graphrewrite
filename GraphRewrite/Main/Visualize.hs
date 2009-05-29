module GraphRewrite.Main.Visualize
    ( renderDot
    ) where

import GraphRewrite.Internal.DeltaFunctions
import GraphRewrite.Internal.RewriteTypes

import Data.GraphViz
import Data.Supply
import Data.Graph.Inductive.Tree
import qualified Data.Graph.Inductive.Graph as IG
import Data.IntMap hiding (map, split)
import Data.Maybe
import Prelude hiding (lookup, exp)

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

graphToGr :: Supply Int -> RewriteSystem -> PointedGraph -> Gr String String
graphToGr ids rs (e, g) = insExpr (-1) ids e IG.empty
    where
      insExpr i ids e@(SRef r) gr = let
          refe = case lookup r g of
                   Just r -> r
                   _ -> case lookup r (rules rs) of
                         Just [r] -> exp r
                         _ -> SLit $ "Undefined reference: " ++ show r
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
      insExpr i ids e@(SApp f args) gr = let
          (idsh:idst) = split ids
          (idsh1, idsh2) = split2 idsh
          (idst1, idst2) = splitAt (length args) idst
          (fnode, _) = if i < 0 then (genLNode idsh2 rs f) else (i,"")
          nodeids = map (\(id,e) -> fst $ genLNode id rs e) (zip idst1 args)
          nodes = foldl (\a (b1,b2,b3) -> insExpr b1 b2 b3 a) (insExpr i idsh1 f gr) (zip3 (reverse nodeids) idst2 (reverse args))
        in
          foldl (\a (b1,b2) -> IG.insEdge (fnode, b1, b2) a) nodes (zip nodeids (map show [1..]))
      insExpr i ids e gr
          | i < 0 = IG.insNode (genLNode ids rs e) gr
          | otherwise = if IG.gelem i gr then gr else IG.insNode (i, genName rs e) gr


grToDot :: Gr String String -> DotGraph
grToDot gr = graphToDot gr []
        (\(_,l) -> [FontColor (RGB 0x1f 0x33 0xb3), FontSize 12, FontName "Helvetica", Label l])
        (\(_,_,l) -> [FontName "Helvetica", ArrowHead Normal, ArrowSize 0.3, Label l])

renderDot :: Supply Int -> RewriteSystem -> PointedGraph -> String
renderDot = (((show . grToDot) .) .) . graphToGr


