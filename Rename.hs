
{-# LANGUAGE FlexibleInstances #-}
{- |
  This module contains functions for transforming a 'Language.Haskell.Parser.ParseResult' to our own representation of a Haskell module.
-}
module Rename
where

import SimpleHaskell

import Data.Supply	-- egyedi azonosítók szétosztásához;  value-supply csomagban

import Language.Haskell.Syntax
import Language.Haskell.Parser

import Control.Monad.Fix

import Data.Map hiding (map, split)
import qualified Data.IntMap as I

import Data.List hiding (lookup, union, insert)
import qualified Data.List as List -- (lookup)
import Data.Maybe

import Prelude hiding (lookup)

-------------------------------------------------

-- | Maps String identifiers to Integer identifiers.
type Binds = Map String Int

-- | Inverse mapping of 'Binds'
type Names = I.IntMap String

-- | An endless supply of Ints
type Ids   = Supply Int

-- hibaüzenet vagy érték
type Maybe' a = Either String a
{-
data Maybe' a
    = Hiba String
    | Ok a
-}

--instance Monad Maybe' where 
instance Monad (Either String) where

	a >>= b = case a of
		Left err	-> fail err
		Right x		-> b x

	fail a = Left a

	return a = Right a

instance MonadFix (Either String) where

    mfix f = m   where

        m = f (kiszed m)

        kiszed (Right a) = a

------------------------------------------------
swap :: (a, b) -> (b, a)
swap = \(x,y) -> (y,x)



-- | Assigns 'Int' identifiers to Strings. Each String gets a unique Int.
distributeIds 
    :: [String]     -- ^ String identifiers
    -> Ids          -- ^ Unique Ints
    -> Maybe' (Binds, Names, [Int]) 
                    -- ^ Error if there were at least two duplicate strings, otherwise returns the newly created assignments (String -> Int and Int -> String) and the list of assigned Ints.
distributeIds l ids = case duplicates l of
	(x:_) -> fail $ "multiply defined: " ++ x
	_	-> return (fromList $ zip l i, I.fromList $ zip i l, i)
 where
	i = take (length l) $ map supplyValue $ split ids

duplicates :: Ord a => [a] -> [a]
duplicates = catMaybes . map (listToMaybe . tail) . group . sort

mapFst f (a,b) = (f a, b)
mapSnd f (a,b) = (a, f b)


------------------------------------------------
-- | Converts a 'ParseResult' as returned by 'Language.Haskell.Parser' to our internal 'SimpModule' format. Returns an empty 'SimpModule' if the parse failed.
convParse :: ParseResult HsModule -> SimpModule String
convParse (ParseFailed _ _) = []
convParse (ParseOk m)       = convModule m

convModule :: HsModule -> SimpModule String
convModule (HsModule _ m exp imp decl) = map convDecl decl

convDecl :: HsDecl -> Decl String
convDecl (HsFunBind hsmatch) = FunBind (map convMatch hsmatch)
convDecl (HsPatBind _ pat rhs decl) = PatBind (convPars pat) (convRhs rhs)

convMatch :: HsMatch -> FunAlt String
convMatch (HsMatch _ (HsIdent fname) pars expr _) = (fname, map convPars pars, convRhs expr)

convPars :: HsPat -> String --FIXME
convPars (HsPVar (HsIdent n)) = n

convRhs :: HsRhs -> Expr String
convRhs (HsUnGuardedRhs expr) = convExpr expr

convExpr :: HsExp -> Expr String
convExpr (HsVar (UnQual (HsIdent var))) = Var var
convExpr (HsApp exp1 exp2)  = Apply [convExpr exp1, convExpr exp2]
convExpr (HsLit literal)    = Lit (show literal)
convExpr (HsLet decls exp)  = Let (map convDecl decls) (convExpr exp)

-------------------------------------------------

-- | Substitutes String identifiers to Int ones in an 'Expr' structure.
renameExpr 
    :: Binds        -- ^ Already assigned Strings
    -> Expr String  -- ^ The expression
    -> Ids          -- ^ An endless supply of unique Ints
    -> Maybe' (Names, Expr Int) -- ^ If substitution is successful, returns the new assignments and the converted expression; otherwise returns an error.
renameExpr b (Lit s) ids = return (I.empty, Lit s)
renameExpr b (Var v) ids = case lookup v b of
	Nothing		-> fail $ "not defined: " ++ v
	Just i		-> return (I.empty, Var i)
renameExpr b (Apply l) ids = fmap (mapSnd Apply) $ renameExprs b l ids
renameExpr b (Let l e) ids = do

  let (ids1, ids2) = split2 ids

  (b', l_names, l') <- renameDecls b l ids1

  (e_names, e') <- renameExpr b' e ids2
  
  return (I.unions [l_names,  e_names], Let l' e')

-- | This is 'renameExpr' for lists. It applies 'renameExpr' for every 'Expr' in the second parameter.
renameExprs :: Binds -> [Expr String] -> Ids -> Maybe' (Names, [Expr Int])
renameExprs b exprs ids = fmap (mapFst I.unions . unzip) $ sequence [renameExpr b e i | (e,i)<- zip exprs (split ids)]

-- | Substitutes String identifiers to Int ones in a 'Decl' structure.
renameDecl  
    :: Binds  -- ^ Already assigned Strings (including function names on the same level)
    -> Decl String  -- ^ The declaration
    -> Ids  -- ^ An endless supply of unique Ints
    -> Maybe' (String, Names, Decl Int )        -- ^ If substitution is successful, returns the name of the 'Decl', the new assignments (this does not include the name of the 'Decl') and the converted declaration; otherwise returns an error.

renameDecl b (FunBind fas@((n,_,_):_)) ids = do

  (names, funalts) <- renameFunAlts b fas ids

  return (n, names, FunBind funalts)

renameDecl b (PatBind p e) ids = do

  (e_names, e') <- renameExpr b e ids

  return (p, e_names, PatBind (b ! p) e')


-- | This is 'renameDecl' for lists. It applies 'renameDecl' to every 'Decl' in the second parameter, properly handling the names of the declarations.
renameDecls :: Binds -> [Decl String] -> Ids 
    -> Maybe' (Binds, Names, [Decl Int])     -- ^ If substitution is successful, returns all of the bindings, the new assignments and the list of converted declarations; otherwise returns an error.
renameDecls b decls ids  = do

    let (ids1, ids2) = split2 ids

    let as = map name decls

    (b_, as_, _) <- distributeIds as ids2

    let b' = union b_ b

    (_as, bs, cs) <- fmap unzip3 $ sequence [renameDecl b' d i | (d,i) <- zip decls (split ids1)]



  
    return (b', I.unions (as_:bs), cs)

name (PatBind p _) = p
name (FunBind ((x,_,_):_)) = x
  
-- | Does the substitution in function alternatives.      
renameFunAlt :: Binds -> FunAlt String -> Ids -> Maybe' (Names, FunAlt Int)
renameFunAlt b (f, as, e) ids = do --elofeltetel: f mar at van nevezve, es b-ben van errol az info
  let (ids1, ids2) = split2 ids

  f' <- case lookup f b of
          Just i  -> return i
          Nothing -> fail $ "This shouldn't happen " ++ f

  (b', as_names, as') <- distributeIds as ids1

  let b'' = union b' b

  (e_names, e') <- renameExpr b'' e ids2

  return (I.union as_names e_names, (f', as', e'))

-- | This is 'renameFunAlt' for lists. It applies 'renameFunAlt' for every 'FunAlt' in the second parameter.
renameFunAlts :: Binds -> [FunAlt String] -> Ids -> Maybe' (Names, [FunAlt Int])
renameFunAlts b funalts ids = fmap (mapFst I.unions . unzip) $ sequence [renameFunAlt b f i | (f,i) <- zip funalts (split ids)]

-------------------------------------------------

renameMain :: Expr String -> Ids -> Maybe' (Names, Expr Int)
renameMain expr ids = renameExpr empty expr ids

-- | Does the substitution in a 'SimpModule' structure.
rename :: SimpModule String -- ^ The module which needs substitution
         -> Ids --  ^ An endless supply of Ints
         -> Maybe' (Names, SimpModule Int) -- ^ If the substitution is successful, returns the assignments for the global identifiers and the converted 'SimpModule'; otherwise returns an error.
rename decls ids = fmap g $ renameDecls empty decls ids
  where g (a,b,c) = (b,c) 

--main = mapM_ test tests
{-
test (Test x z) = do
	putStrLn "------------------------------- Test"
	print x
	putStrLn "  ==> "
	ids <- newEnumSupply
	case renameMain x ids of
		Left err	->	case z of
				Just err' | err == err' 	-> putStrLn $ "Expected error: " ++ err
				_ 	-> putStrLn $ "!Unexpected error: " ++ err
		Right (names, y)	-> case z of
			Nothing	-> do
				print y
--				putStrLn $ "invRename test: " ++ if invRename names y == x then "OK" else "FAILED!"
			Just err	-> do
				putStrLn $ "!Expected error " ++ err ++ " but got"
				print y

data Test = Test (Expr String) (Maybe String)
-}
