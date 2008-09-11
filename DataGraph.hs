--Data.Bimap

module DataGraph where

import Data.IntMap
import Language.Haskell.Syntax
import Language.Haskell.Parser

data Expr = Lit Literal
          | Const Int
          | App Expr Expr
          | Fun Int
          | Var Int
          | Wildcard
            deriving Show

type Literal = String

type Rule = (Expr, Expr)

data DataGraph = Textual { 
      main :: Int,
      functions :: IntMap [Rule],
--      expressions :: IntMap Expr,
      names :: IntMap String }

convParse :: ParseResult HsModule -> Maybe DataGraph
convParse (ParseFailed _ _) = Nothing
convParse (ParseOk a) = Just (convModule a)

convModule :: HsModule -> DataGraph
convModule (HsModule srcloc mod exportspec importdecl decl) 
    = Textual (foldl process empty decl) vars
      where
        process :: (IntMap Expr) -> HsDecl -> (IntMap Expr)
        process map item = insert ((lastWithDef 0 (keys map)) + 1) (convDecl vars item) map
        lastWithDef a [] = a
        lastWithDef _ l = last l
        vars = fromList$zip [1..(length varlist)] varlist -- TODO
        varlist = Prelude.filter (\x -> x /= "") (Prelude.map getVarName decl)

getVarName :: HsDecl -> String
getVarName (HsPatBind srcloc (HsPVar name) rhs decls)
    = getVarNameFromHsName name
getVarName (HsFunBind (h:t))
    = case h of
        HsMatch _ name _ _ _ -> getVarNameFromHsName name                                 
getVarName _ = ""

getVarNameFromHsName name = case name of
                              HsIdent  i -> i
                              HsSymbol s -> s

convDecl :: IntMap String -> HsDecl -> Expr
convDecl _ _ = Var 3 -- TODO
