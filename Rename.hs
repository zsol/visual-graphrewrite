
{-# LANGUAGE FlexibleInstances #-}

import Data.Supply	-- egyedi azonosítók szétosztásához;  value-supply csomagban

import Data.Map hiding (map, split)

import Data.Maybe
import Data.List hiding (lookup, union)
import Prelude hiding (lookup)


-------------------------------------------------

data Expr a 
	= Let [(a, Expr a)] (Expr a)
	| Var a
	| Apply [Expr a]
	| Lit String
		deriving (Show, Eq)

type Binds = Map String Int
type Names = Map Int String
type Ids   = Supply Int

-------------------------------------------------

-- hibaüzenet vagy érték
type Maybe' a = Either String a

instance Monad (Either String) where

	a >>= b = case a of
		Left err	-> fail err
		Right x		-> b x

	fail a = Left a

	return a = Right a

------------------------------------------------- auxiliary

-- megmondja hogy miből van több
duplicates :: Ord a => [a] -> [a]
duplicates = catMaybes . map (listToMaybe . tail) . group . sort

mapFst f (a,b) = (f a, b)
mapSnd f (a,b) = (a, f b)

-------------------------------------------------

-- a legfontosabb függvény
rename :: Binds -> Expr String -> Ids -> Maybe' (Names, Expr Int)
rename b (Lit s) ids = return (empty, Lit s)
rename b (Var v) ids = case lookup v b of
	Nothing		-> fail $ "not defined: " ++ v
	Just i		-> return (empty, Var i)
rename b (Apply l) ids = fmap (mapSnd Apply) $ renames b l ids
rename b (Let l e) ids = do

	let (lhss, rhss) = unzip l
	let (ids1, ids2, ids3) = split3 ids

	(b', lhss_names, lhss') <- distributeIds lhss ids1

	let b'' = union b' b		-- itt fedjük el a felsőbb neveket

	(rhss_names, rhss') <- renames b'' rhss ids2
	(e_names, e') <- rename b'' e ids3

	return (unions [lhss_names, rhss_names, e_names], Let (zip lhss' rhss') e')

renames :: Binds -> [Expr String] -> Ids -> Maybe' (Names, [Expr Int])
renames b exprs ids = fmap (mapFst unions . unzip) $ sequence [rename b e i | (e,i)<- zip exprs (split ids)]

distributeIds :: [String] -> Ids -> Maybe' (Binds, Names, [Int])
distributeIds l ids = case duplicates l of
	(x:_) -> fail $ "multiply defined: " ++ x
	_	-> return (fromList $ zip l i, fromList $ zip i l, i)
 where
	i = take (length l) $ map supplyValue $ split ids


-------------------------------------------------

invRename :: Names -> Expr Int -> Expr String
invRename names expr = f expr  where

	f (Let l e) = Let [(names ! a, f b) | (a,b)<-l] (f e)
	f (Apply l) = Apply (map f l)
	f (Var v) = Var (names ! v)
	f (Lit s) = Lit s

-------------------------------------------------

renameMain :: Expr String -> Ids -> Maybe' (Names, Expr Int)
renameMain expr ids = rename empty expr ids

main = mapM_ test tests

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
				putStrLn $ "invRename test: " ++ if invRename names y == x then "OK" else "FAILED!"
			Just err	-> do
				putStrLn $ "!Expected error " ++ err ++ " but got"
				print y

data Test = Test (Expr String) (Maybe String)

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



