{- |
   This module contains functions for converting a 'Language.Haskell.Parser.ParseResult'
   to our own representation of a Haskell module called 'SimpleHaskell'.
-}

module Convert
where

import Data.List
import Data.Char

import SimpleHaskell
import Language.Haskell.Syntax
import Language.Haskell.Parser

-- | Converts a 'ParseResult' as returned by 'Language.Haskell.Parser' to our internal 'SimpModule' format. Returns an empty 'SimpModule' if the parse failed.
convParse :: ParseResult HsModule -> SimpModule String
convParse (ParseFailed _ _) = []
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
convMatch (HsMatch _ (HsIdent fname) pars expr _) = (fname, map convPars pars, convRhs expr)

convPars :: HsPat -> Patt String
convPars (HsPVar n) = convName n
convPars (HsPLit n) = Lit (convLit n)
convPars (HsPApp qn l) = Apply $ (convQName qn) : (map convPars l)
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

convExpr :: HsExp -> Expr String
convExpr (HsVar qn) = convQName qn
convExpr (HsApp exp1 exp2)  = Apply [convExpr exp1, convExpr exp2]
convExpr (HsInfixApp exp1 (HsQVarOp qn) exp2) = Apply [convQName qn, convExpr exp1, convExpr exp2]
convExpr (HsInfixApp exp1 (HsQConOp qn) exp2) = Apply [convQName qn, convExpr exp1, convExpr exp2]
convExpr (HsLit literal)    = Lit (convLit literal)
convExpr (HsLet decls exp)  = Let (map convDecl decls) (convExpr exp)
convExpr (HsCon (UnQual (HsIdent con))) = Cons con
convExpr (HsParen exp) = convExpr exp
convExpr (HsList []) = Cons "[]"
convExpr a = Lit ("UNIMPLEMENTED: " ++ show a)
