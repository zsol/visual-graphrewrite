{- |
  This module contains functions for assigning integer identifiers to lexical elements of
  a Haskell source file. This process is called renaming.
-}
module Rename
where

import SimpleHaskell
import Convert

import Data.Supply

import Language.Haskell.Syntax
import Language.Haskell.Parser

import Control.Monad.Fix

import Data.Map hiding (map, split, filter)
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
type UniqueIds   = Supply Int

-- type Id = Int

-- | Result monad; error spreads
data Result a
    = Hiba String
    | Ok a
      deriving (Show, Eq)


--instance Monad Result where 
instance Monad (Result) where

	a >>= b = case a of
		Hiba err	-> fail err
		Ok x		-> b x

	fail a = Hiba a

	return a = Ok a

instance MonadFix (Result) where

    mfix f = m   where

        m = f (kiszed m)

        kiszed (Ok a) = a

instance Functor Result where
    
    fmap f (Ok a) = Ok (f a)
    fmap f (Hiba err) = Hiba err
------------------------------------------------
swap :: (a, b) -> (b, a)
swap = \(x,y) -> (y,x)

-- | Assigns 'Int' identifiers to Strings. Each String gets a unique Int.
distributeIds 
    :: [String]     -- ^ String identifiers
    -> UniqueIds          -- ^ Unique Ints
    -> Result (Binds, Names, [Int]) 
                    -- ^ Error if there were at least two duplicate strings, otherwise returns the newly created assignments (String -> Int and Int -> String) and the list of assigned Ints.
distributeIds l ids = case duplicates l of
	                (x:_) -> fail $ "multiple definition: " ++ x
	                _     -> return (fromList $ zip l i, I.fromList $ zip i l, i)
                            where
	                      i = take (length l) $ map supplyValue $ split ids

-- | Does the same thing as distributeIds, only for [[String]] lists
distributeIds' :: [[String]] -> UniqueIds -> Result (Binds, Names, [[Int]])
distributeIds' (l:ls) ids = do
  let (ids1, ids2) = split2 ids
  (b, n, i) <- distributeIds l ids1
  (b', n', i') <- distributeIds' ls ids2
  return (union b b', I.union n n', i:i')

distributeIds' [] _ = return (empty, I.empty, [[]])

-- | Find the duplicates in the first argument
duplicates :: Ord a => [a] -> [a]
duplicates = catMaybes . map (listToMaybe . tail) . group . sort

mapFst f (a,b) = (f a, b)
mapSnd f (a,b) = (a, f b)

-- | Substitutes String identifiers to Int ones in an 'Expr' structure.
renameExpr 
    :: Binds        -- ^ Already assigned Strings
    -> Expr String  -- ^ The expression
    -> UniqueIds          -- ^ An endless supply of unique Ints
    -> Result (Names, Expr Int) -- ^ If substitution is successful, returns the new assignments and the converted expression; otherwise returns an error.
renameExpr b (Lit s) ids = return (I.empty, Lit s)
renameExpr b (Var v) ids = case lookup v b of
	Nothing		-> fail $ "not defined: " ++ v
	Just i		-> return (I.empty, Var i)
renameExpr b (Apply l) ids = fmap (mapSnd Apply) $ renameExprs b l ids
renameExpr b (Cons c) ids = case lookup c b of
                              Nothing -> fail $ "not defined: " ++ c
                              Just i  -> return (I.empty, Cons i)
renameExpr b (Let l e) ids = do

  let (ids1, ids2) = split2 ids

  (b', l_names, l') <- renameDecls b l ids1

  (e_names, e') <- renameExpr b' e ids2
  
  return (I.unions [l_names,  e_names], Let l' e')

-- | This is 'renameExpr' for lists. It applies 'renameExpr' for every 'Expr' in the second parameter.
renameExprs :: Binds -> [Expr String] -> UniqueIds -> Result (Names, [Expr Int])
renameExprs b exprs ids = fmap (mapFst I.unions . unzip) $ sequence [renameExpr b e i | (e,i)<- zip exprs (split ids)]

-- | Substitutes String identifiers to Int ones in a 'Decl' structure.
renameDecl  
    :: Binds  -- ^ Already assigned Strings (including function names on the same level)
    -> Decl String  -- ^ The declaration
    -> UniqueIds  -- ^ An endless supply of unique Ints
    -> Result (String, Names, Decl Int )        -- ^ If substitution is successful, returns the name of the 'Decl', the new assignments (this does not include the name of the 'Decl') and the converted declaration; otherwise returns an error.

renameDecl b (FunBind fas@((n,_,_):_)) ids = do

  (names, funalts) <- renameFunAlts b fas ids

  return (n, names, FunBind funalts)

renameDecl b (PatBind p e) ids = do

  (e_names, e') <- renameExpr b e ids

  return (head $ nameExpr p, e_names, PatBind (joinPatts (p, [(b ! (head $ nameExpr p))])) e')

renameDecl b (DataDecl a) ids = Ok (a, I.empty, DataDecl (-1))


-- | This is 'renameDecl' for lists. It applies 'renameDecl' to every 'Decl' in the second parameter, properly handling the names of the declarations.
renameDecls :: Binds -> [Decl String] -> UniqueIds 
    -> Result (Binds, Names, [Decl Int])     -- ^ If substitution is successful, returns all of the bindings, the new assignments and the list of converted declarations; otherwise returns an error.
renameDecls b decls ids  = do

    let (ids1, ids2) = split2 ids

    let as = map name decls

    (b_, as_, _) <- distributeIds as ids2

    let b' = union b_ b

    (_as, bs, cs) <- fmap unzip3 $ sequence [renameDecl b' d i | (d,i) <- zip decls (split ids1)]
  
    return (b', I.unions (as_:bs), cs)

--name :: (Read a) => Decl a -> a
--name (PatBind n  _) = head $nameExpr n
--name (FunBind ((x,_,_):_)) = x
--name (DataDecl a) = a

--nameExpr :: (Read a) => Expr a -> [a]
--nameExpr (Var n) = [n]
--nameExpr (Cons n) = [n]
--nameExpr (Lit n) = [read n]
--nameExpr (Apply es) = concat $ map nameExpr es

-- | Substitutes the String identifiers in a pattern with the supplied integer(s).
joinPatts :: (Patt String, [Int]) -> Patt Int
joinPatts (Var _, [i]) = Var i
joinPatts (Cons _, [i]) = Cons i
joinPatts (Lit s, _) = Lit s
joinPatts (Apply s, i) = Apply (map joinPatts (zip s i'))
    where
      i' = map (\x -> [x]) i
                         
  
-- | Does the substitution in function alternatives.      
renameFunAlt :: Binds -> FunAlt String -> UniqueIds -> Result (Names, FunAlt Int)
renameFunAlt b (f, as, e) ids = do --elofeltetel: f mar at van nevezve, es b-ben van errol az info
  let (ids1, ids2) = split2 ids

  f' <- case lookup f b of
          Just i  -> return i
          Nothing -> fail $ "This shouldn't happen " ++ f

  let as' = filter cons as 

  (b', as_names, as'') <- distributeIds' (map nameExpr as') ids1

  let b'' = union b' b

  (e_names, e') <- renameExpr b'' e ids2

  return (I.union as_names e_names, (f', (map joinPatts (zip as' as'')), e'))
    where
      cons (Cons _) = False
      cons _ = True

-- | This is 'renameFunAlt' for lists. It applies 'renameFunAlt' for every 'FunAlt' in the second parameter.
renameFunAlts :: Binds -> [FunAlt String] -> UniqueIds -> Result (Names, [FunAlt Int])
renameFunAlts b funalts ids = fmap (mapFst I.unions . unzip) $ sequence [renameFunAlt b f i | (f,i) <- zip funalts (split ids)]

-------------------------------------------------

-- | Does the substitution in a 'SimpModule' structure.
rename :: Binds -- ^ Predefined entities
       -> SimpModule String -- ^ The module which needs substitution
       -> UniqueIds --  ^ An endless supply of Ints
       -> Result (Names, SimpModule Int) -- ^ If the substitution is successful, returns the assignments for the global identifiers and the converted 'SimpModule'; otherwise returns an error.
rename predef decls ids = fmap g $ renameDecls predef decls ids
  where g (a,b,c) = (b,c) 
