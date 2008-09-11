
{-# LANGUAGE FlexibleInstances #-}
module Rename 
where

import Data.Supply	-- egyedi azonosítók szétosztásához;  value-supply csomagban

import Data.Map hiding (map, split)

import Data.Maybe
import Data.List hiding (lookup, union, insert)
import Prelude hiding (lookup)

import qualified Data.IntMap as I
import qualified Data.List (lookup)

import Language.Haskell.Syntax
import Language.Haskell.Parser

-------------------------------------------------

type Module' a = [Decl a]

data Decl a
    = FunBind [FunAlt a]
    | PatBind a (Expr a)
      deriving (Show, Eq)

type FunAlt a = (a, [a], (Expr a))

data Expr a 
	= Let [Decl a] (Expr a)
	| Var a
	| Apply [Expr a]
	| Lit String
		deriving (Show, Eq)

type Binds = Map String Int
type Names = I.IntMap String
type Ids   = Supply Int

-- hibaüzenet vagy érték
type Maybe' a = Either String a

instance Monad (Either String) where

	a >>= b = case a of
		Left err	-> fail err
		Right x		-> b x

	fail a = Left a

	return a = Right a

------------------------------------------------
swap :: (a, b) -> (b, a)
swap = \(x,y) -> (y,x)

namesToBinds :: Names -> Binds
namesToBinds = fromList . (map swap) . I.toList

bindsToNames :: Binds -> Names
bindsToNames = I.fromList . (map swap) . toList

distributeIds :: [String] -> Ids -> Maybe' (Binds, Names, [Int])
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

convParse :: ParseResult HsModule -> Module' String
convParse (ParseFailed _ _) = []
convParse (ParseOk m)       = convModule m

convModule :: HsModule -> Module' String
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

-- a legfontosabb függvény
renameExpr :: Binds -> Expr String -> Ids -> Maybe' (Names, Expr Int)
renameExpr b (Lit s) ids = return (I.empty, Lit s)
renameExpr b (Var v) ids = case lookup v b of
	Nothing		-> fail $ "not defined: " ++ v
	Just i		-> return (I.empty, Var i)
renameExpr b (Apply l) ids = fmap (mapSnd Apply) $ renameExprs b l ids
renameExpr b (Let l e) ids = do
  let (ids1, ids2) = split2 ids

  (l_names, l') <- renameDecls' b l ids1

  let b' = namesToBinds l_names
  let b'' = union b' b

  (e_names, e') <- renameExpr b'' e ids2
  
  return (I.unions [l_names,  e_names], Let l' e')

renameExprs :: Binds -> [Expr String] -> Ids -> Maybe' (Names, [Expr Int])
renameExprs b exprs ids = fmap (mapFst I.unions . unzip) $ sequence [renameExpr b e i | (e,i)<- zip exprs (split ids)]

--Ugyanaz, mint fent, csak a sorrend szamit
renameExprs' :: Binds -> [Expr String] -> Ids -> Maybe' (Names, [Expr Int])
renameExprs' b (eh:es) ids = do
  let (ids1, ids2) = split2 ids

  (eh_names, eh') <- renameExpr b eh ids1

  let b' = union (namesToBinds eh_names) b

  (es_names, es') <- renameExprs' b' es ids2

  return (I.union eh_names es_names, (eh':es'))

renameExprs' _ [] _ = Right (I.empty, [])

renameDecl  :: Binds ->  Decl String  -> Ids -> Maybe' (Names,  Decl Int )
renameDecl b (FunBind fas) ids = do
  let (ids1, ids2) = split2 ids

  let fs = map (\x@(x1, x2, x3) -> x1) fas 
  (b', f_names, ufs') <- distributeIds (nub fs) ids1

  let b'' = union b' b
  
  (names, funalts) <- renameFunAlts' b'' fas ids2

  return (I.union f_names names, FunBind funalts)

renameDecl b (PatBind p e) ids = do
  let (ids1, ids2) = split2 ids
  let p' = supplyValue ids1
  let b' = insert p p' b

  (e_names, e') <- renameExpr b' e ids2

  return (I.insert p' p e_names, PatBind p' e')

  
      
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

renameFunAlts :: Binds -> [FunAlt String] -> Ids -> Maybe' (Names, [FunAlt Int])
renameFunAlts b funalts ids = fmap (mapFst I.unions . unzip) $ sequence [renameFunAlt b f i | (f,i) <- zip funalts (split ids)]

--Mint az elozo, csak a sorrend szamit
renameFunAlts' :: Binds -> [FunAlt String] -> Ids -> Maybe' (Names, [FunAlt Int])
renameFunAlts' b (fh:fs) ids = do
  let (ids1, ids2) = split2 ids
                     
  (fh_names, fh') <- renameFunAlt b fh ids1

  let b' = union (namesToBinds fh_names) b

  (fs_names, fs') <- renameFunAlts' b' fs ids2

  return (I.union fh_names fs_names, (fh':fs'))

renameFunAlts' _ [] _ = Right (I.empty, [])
  
renameDecls :: Binds -> [Decl String] -> Ids -> Maybe' (Names, [Decl Int])
renameDecls b decls ids = fmap (mapFst I.unions . unzip) $ sequence [renameDecl b d i | (d,i) <- zip decls (split ids)]

renameDecls' :: Binds -> [Decl String] -> Ids -> Maybe' (Names, [Decl Int])
renameDecls' b (dh:ds) ids = do
  let (ids1, ids2) = split2 ids

  (dh_names, dh') <- renameDecl b dh ids1

  let b' = union (namesToBinds dh_names) b

  (ds_names, ds') <- renameDecls' b' ds ids2

  return (I.union dh_names ds_names, (dh':ds'))

renameDecls' _ [] _ = Right (I.empty, [])

-------------------------------------------------
{-
invRename :: Names -> Expr Int -> Expr String
invRename names expr = f expr  where

	f (Let l e) = Let [(names I.! a, f b) | (a,b)<-l] (f e)
	f (Apply l) = Apply (map f l)
	f (Var v) = Var (names I.! v)
	f (Lit s) = Lit s
-}
-------------------------------------------------

renameMain :: Expr String -> Ids -> Maybe' (Names, Expr Int)
renameMain expr ids = renameExpr empty expr ids

rename :: Module' String -> Ids -> Maybe' (Names, Module' Int)
rename decls ids = renameDecls' empty decls ids
  

--main = mapM_ test tests

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
{-
tests =
	[ Test
		(Let
			[ ("x", Lit "Hello")
			, ("x", Lit "")
			] (Lit ""))
		(Just "multiply defined: x")
	, Test
		(Let
			[ ("x", Var "y")
			, ("y", Lit "yy")
			] (Var "x"))
		Nothing
	, Test
		(Var "z")
		(Just "not defined: z")
	-- az elfedés tesztelésére
	, Test
		(Let
			[ ("x", Lit "Hi")
			] (Let
				[ ("x", Lit "Ok")
				] (Var "x")))
		Nothing
	]
-}


