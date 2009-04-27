module Visualize
where

import qualified SimpleHaskell as SH
import RewriteTypes

import Text.PrettyPrint

import Control.Applicative

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

exprToGraph :: RewriteSystem -> Expr -> VisGraph
exprToGraph rs e@(SApp f es) = (genLabel rs f, map exprID es) : concatMap (exprToGraph rs) es
exprToGraph rs e = [(genLabel rs e, [])]

genLabel :: RewriteSystem -> Expr -> String
genLabel rs (SCons c) = show c ++ "|" ++ lkp rs c
genLabel rs (SFun _ f) = show f ++ "|" ++ lkp rs f
genLabel _ (SLit l) = l
genLabel rs (SHole h) = show h ++ "|" ++ lkp rs h
genLabel rs (SRef r) = show r ++ "|" ++ lkp rs r
genLabel rs (SApp e es)  = unwords $ (genLabel rs e) : map (genLabel rs) es



lkp rs i = fromMaybe "UNDEFINED" $ lookup i (names rs)

