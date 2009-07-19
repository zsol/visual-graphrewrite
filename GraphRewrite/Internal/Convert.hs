{- |
   This module contains functions for converting a 'Language.Haskell.Parser.ParseResult'
   to our own representation of a Haskell module called 'SimpleHaskell'.
-}

module GraphRewrite.Internal.Convert
where

import GraphRewrite.Internal.SimpleHaskell

import Data.List
import Data.Char

import Language.Haskell.Syntax
import Language.Haskell.Parser

-- | Converts a 'ParseResult' as returned by 'Language.Haskell.Parser' to our internal 'SimpModule' format. Returns an empty 'SimpModule' if the parse failed.
convParse :: ParseResult HsModule -> SimpModule String
convParse (ParseFailed loc err) = error $ "Parse of module failed at " ++ show loc ++ " with message: " ++ err
convParse (ParseOk m)       = convModule m

splitDecls :: [HsDecl] -> ([HsDecl], [HsDecl])
splitDecls l = partition isEnvRelated l
    where
      isEnvRelated d = case d of
                         (HsDataDecl _ _ _ _ _ _) -> True
                         (HsTypeDecl _ _ _ _) -> True
                         _ -> False


convModule :: HsModule -> SimpModule String
convModule (HsModule _ _m _exp _imp decl) = map convDecl (snd $ splitDecls decl)

convDecl :: HsDecl -> Decl String
convDecl (HsFunBind hsmatch) = FunBind (map convMatch hsmatch)
convDecl (HsPatBind _ pat rhs _decl) = PatBind (convPars pat) (convRhs rhs)
--convDecl (HsDataDecl _ _ (HsIdent a) _ _ _) = DataDecl a
--convDecl (HsDataDecl _ _ (HsSymbol a) _ _ _) = DataDecl a

convMatch :: HsMatch -> FunAlt String
convMatch (HsMatch _ fname pars expr _) = (head $ nameExpr $ convName fname, map convPars pars, convRhs expr)

convPars :: HsPat -> Patt String
convPars (HsPVar n) = convName n
convPars (HsPLit n) = Lit (convLit n)
convPars (HsPApp qn l) = Apply $ convQName qn : map convPars l
convPars (HsPParen p) = convPars p
convPars (HsPAsPat n p) = AsPat (head $ nameExpr $ convName n) (convPars p)
--convPars (HsPNeg n) --TBD
convPars (HsPList []) = Cons "[]"
convPars (HsPInfixApp p1 p2 p3) = convPars (HsPApp p2 [p1,p3])
convPars p = error $ "convPars: " ++ show p

convName :: HsName -> Expr String
convName (HsIdent  n)
    | isUpper (head n) = Cons n
    | otherwise        = Var n
convName (HsSymbol n) = Var n --FIXME

convQName :: HsQName -> Expr String
convQName (UnQual n) = convName n
--convQName Qual m n
convQName (Special HsCons) = Cons ":"
convQName (Special HsListCon) = Cons "[]"


convLit :: HsLiteral -> String
convLit (HsChar n) = show n
convLit (HsString n) = show n
convLit (HsInt n) = show n
convLit (HsFrac n) = show n
convLit (HsCharPrim n) = show n
convLit (HsStringPrim n) = show n
convLit (HsIntPrim n) = show n
convLit (HsFloatPrim n) = show n
convLit (HsDoublePrim n) = show n

convRhs :: HsRhs -> Expr String
convRhs (HsUnGuardedRhs expr) = convExpr expr

convQOp :: HsQOp -> Expr String
convQOp (HsQVarOp qn) = convQName qn
convQOp (HsQConOp qn) = convQName qn

convExpr :: HsExp -> Expr String
convExpr (HsVar qn) = convQName qn
convExpr (HsApp exp1 exp2)  = Apply [convExpr exp1, convExpr exp2]
convExpr (HsInfixApp exp1 op exp2) = Apply [convQOp op, convExpr exp1, convExpr exp2]
convExpr (HsLit literal)    = Lit (convLit literal)
convExpr (HsLet decls exp)  = Let (map convDecl decls) (convExpr exp)
convExpr (HsCon (UnQual (HsIdent con))) = Cons con
convExpr (HsParen exp) = convExpr exp
convExpr (HsList []) = Cons "[]"
convExpr (HsList (h:t)) = Apply [Cons ":", convExpr h, convExpr (HsList t)]
convExpr (HsTuple exps) = Apply $ Cons "Tuple" : map convExpr exps
convExpr (HsLeftSection exp op) = Apply [convQOp op, convExpr exp]
--convExpr (HsLambda loc pats exp) = convExpr $ HsLet [HsFunBind [HsMatch loc "lambda" pats (HsUnGuardedRhs exp) []]] (HsVar $ UnQual $ HsIdent "lambda")
--convExpr (HsRightSection op exp) = convExpr $ HsLambda emptyLoc [HsPVar $ HsIdent "x"]
convExpr a = Lit ("UNIMPLEMENTED: " ++ show a)

emptyLoc :: SrcLoc
emptyLoc = SrcLoc "dummy" 0 0
