
--module Test  where

import Rewrite
import RewriteTypes hiding (PointedGraph)

import System.IO.Unsafe
import Test.ChasingBottoms.TimeOut
import Test.LazySmallCheck
import qualified Data.IntMap as IM
import Data.List

{-
Serial instance needed:
    Graph  
    RewriteSystem

-}


--------------------------- helper functions

infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
(<=>) = (==)

sameSet :: Ord a => [a] -> [a] -> Bool
sameSet a b = sort a == sort b

timeOut2s :: a -> IO a
timeOut2s x = do
    r <- timeOut 2 (return x)
    case r of
        NonTermination      -> error "nontermination"
        Value x             -> return x
        Exception e         -> error (show e)

--------------------------- Serial instances

instance Serial Expr where
    series 
        =  cons1 SCons 
        \/ cons2 SFun 
        \/ cons1 SLit 
        \/ cons1 SHole 
        \/ cons1 (SRef . natToInt) 
        \/ cons2 SApp

newtype DataExpr 
    = DataExpr { unDataExpr :: Expr }
        deriving Show

instance Serial DataExpr where
    series 
        =  cons1 (DataExpr . SCons) 
        \/ cons2 (\x -> DataExpr . SFun x) 
        \/ cons1 (DataExpr . SLit)
        \/ cons1 (DataExpr . SRef . natToInt)
        \/ cons2 (\(DataExpr x) ys -> DataExpr $ SApp x $ map unDataExpr ys)

newtype DataGraph 
    = DataGraph Graph
        deriving Show

instance Serial DataGraph where
    series = cons1 (DataGraph . IM.fromList . zip [0..] . map unDataExpr)

data Nat
    = Zero
    | Succ Nat
        deriving (Eq, Ord, Show)

instance Num Nat where
    fromInteger 0 = Zero
    fromInteger n | n < 0 = error "fromInteger/Num"
    fromInteger n = Succ (fromInteger (n-1))

    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined

instance Serial Nat where
    series = cons0 Zero \/ cons1 Succ

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

---------------------------------

data PointedGraph 
    = PGraph Expr Graph
        deriving (Show)

instance Serial PointedGraph where
    series = cons1 mkDataGraph

mkDataGraph :: Nat -> PointedGraph
mkDataGraph n = PGraph (SLit "15") IM.empty

--------------------------- auxiliary predicates and functions

dataExpr :: Expr -> Bool
dataExpr (SHole _)   = False
dataExpr (SApp e es) = dataExpr e && all dataExpr es
dataExpr _           = True

dataGraph :: Graph -> Bool
dataGraph g = all dataExpr (IM.elems g)

ref :: Expr -> Bool
ref (SRef _) = True
ref _        = False

collectRefs :: Expr -> [Int]
collectRefs (SRef i)    = [i] 
collectRefs (SApp e es) = collectRefs e ++ concatMap collectRefs es
collectRefs _           = []

allReferenceDefined :: Expr -> Graph -> Bool
allReferenceDefined e g = all (`IM.member` g) (collectRefs e ++ concatMap collectRefs (IM.elems g))

---------------------------

main :: IO ()
main = do

    let 
        p (SCons _)  = True
        p (SFun _ _) = True
        p (SLit _)   = True
        p _          = False

    smallCheck 30 $ \(PGraph e g) -> -- unsafePerformIO $ timeOut2s $
            not (ref e) && allReferenceDefined e g  ==>  p (fst (flattenSApp e g))

{-
    sm $ interpreter "start = f True; f x = x" == "True"

    sm $ \s1 s2 s3 ->   s1 /= s2  
            ==>  freeVars (s1 ++ " = 3;" ++ s2 ++ " " ++ s3 ++ " = " ++ s3) 
                    `sameSet` [s1, s2]
-}


