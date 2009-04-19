
module Rewrite
    ( rewriteHNF, rewriteStep, rewriteStep' )
where

  import RewriteTypes
  import qualified SimpleHaskell as SH
  import DeltaFunctions

  import qualified Data.IntMap as I
  import qualified Data.List as L

  import Data.Maybe
  import Prelude hiding (exp)

  data Result = OK | Fail

-------------------- eliminate SRef

  deref :: Expr -> I.IntMap Expr -> Expr
  deref (SRef ref) im = case I.lookup ref im of
                          Just e  -> deref e im
                          Nothing -> error "deref" -- SRef ref
  deref e _ = e

-------------------- flatten SApp in first arguments

  flattenSApp :: Expr -> Graph ->
    ( Expr          -- csak SFun, SCons, SLit lehet
    , [Expr])       -- SFun estén lehet nem üres, SCons, SLit esetén üres
  flattenSApp (SApp x xs) g
    = case deref x g of
      SApp y ys  -> flattenSApp (SApp y (ys ++ xs)) g
      x          -> (x, xs)
  flattenSApp x _       -- SLit, SCons, SFun  esetén
    = (x, [])


--------------------

  -- | Rewrite an expression to it's Head Normal Form.
  rewriteHNF
      :: RewriteSystem -- ^ A rewrite system which contains rules
      -> Expr          -- ^ Expression to be rewritten
      -> Graph         -- ^ Graph showing images of references
      -> PointedGraph  -- ^ Resulting HNF expression with the hopefully empty graph.
  rewriteHNF rs e g = case rewriteStep rs e g of
                        Nothing     -> (e, g)
                        Just (e, g) -> rewriteHNF rs e g

  -- | Does a rewrite step on the specified expression or returns the original (Expr, Graph) pair. See also 'rewriteStep'.
  rewriteStep' :: RewriteSystem -> Expr -> Graph -> PointedGraph
  rewriteStep' rs e g = fromMaybe (e, g) $ rewriteStep rs e g


  -- | Does a rewrite step on the specified expression maybe returning the result.
  rewriteStep
      :: RewriteSystem -- ^ A rewrite system which contains rules
      -> Expr          -- ^ Expression to be rewritten
      -> Graph         -- ^ Graph showing images of references
      -> Maybe PointedGraph -- ^ Just the resulting pointed graph or Nothing if rewriting is impossible.
  rewriteStep rs e g =
      case flattenSApp (deref e g) g of
        (SFun ar f, l) -> case rls of
                           Just rls
                               | length l == ar -> firstMatch rs g l rls
                               | length l  > ar -> do
                                       (e, g) <- firstMatch rs g (take ar l) rls
                                       pg <- rewriteStep rs (SApp e (drop ar l)) g
                                       return pg
                               | otherwise      -> Nothing
                           Nothing -> do
                                    let l' = map (fst . rewriteExp) l
                                    f' <- I.lookup f (names rs)
                                    e <- rewriteDelta f' l'
                                    -- a vegeredmeny literalra atiranyitani azokat az eleket, amik a delta fuggvenyre mutattak
                                    return (e, I.empty)
            where
                rls = (I.lookup f (rules rs))
                rewriteExp = (flip (rewriteHNF rs)) g

        _ -> Nothing
  -- | Gets the first matching rule for a list of patterns (function arguments).
  firstMatch :: RewriteSystem -> Graph -> [Expr] -> [Rule] -> Maybe PointedGraph
  firstMatch _ _ _ [] = Nothing
  firstMatch rs g es (rule:rules)
     = case matches rs g es (patts rule) I.empty  of
         (g, Just bs) -> Just (substitute bs (exp rule), g)
         _            -> firstMatch rs g es rules

  -- | Does the rewriting on a delta function and its arguments.
  rewriteDelta :: String -> [Expr] -> Maybe Expr
  rewriteDelta f l = do
    l' <- mapM deLit l
    return (SLit $ evalDelta f l')
      where
        deLit :: Expr -> Maybe String
        deLit (SLit a) = Just a
        deLit _        = Nothing

  -- | Substitutes 'SRef' structures to its images. This is a deep implementation which calls itself recursively for 'SApp'.
  substitute
      :: I.IntMap Expr -- mit mire
      -> Expr          -- miben
      -> Expr
  substitute bs (SRef n) = fromMaybe (error "Internal error: reference target not found") $ I.lookup n bs
  substitute bs (SApp e es) = SApp (substitute bs e) (map (substitute bs) es)
  substitute bs e = e

  matches
    :: RewriteSystem
    -> Graph
    -> [Expr]         -- ^ Expressions
    -> [Expr]         -- ^ Patterns
    -> I.IntMap Expr  -- ^ Binds
    -> (Graph, Maybe (I.IntMap Expr))

  matches rs g [] [] bs = (g, Just bs)
  matches rs g (e:es) (p:ps) bs
    = case match rs g e p bs of
        (g, Just bs)    -> matches rs g es ps bs
        x               -> x


  match
    :: RewriteSystem
    -> Graph
    -> Expr           -- ^ Expression
    -> Expr           -- ^ Pattern
    -> I.IntMap Expr  -- ^ Binds
    -> (Graph, Maybe (I.IntMap Expr))
  match rs g e (SHole n) bs = (g, Just (I.insert n e bs))
  match rs g e (SLit y)  bs
        = case rewriteStep' rs e g of
            (SLit x, g)  | x == y      -> (g, Just bs)
            _                          -> (g, Nothing)
  match rs g e (SCons y) bs
        = case rewriteStep' rs e g of
            (SCons x, g)  | x == y     -> (g, Just bs)
            _                          -> (g, Nothing)

  match rs g e (SApp y ys) bs
        = case rewriteStep' rs e g of
            (SApp x xs, bs)            -> matches rs g (x:xs) (y:ys) bs
            _                          -> (g, Nothing)


{-

Apply [Apply [Var "++", Apply [Var "showInt", Apply [Apply [Var "div", Var "n"], Lit "10"]]],
       Apply [Var "showInt", Apply [Apply [Var "mod", Var "n"], Lit "10"]]]

( 1 -> "++", 2 -> "showInt", 3 -> "div", 4 -> "n", 5 -> "mod" )

-->

Apply [Apply [Var 1, Apply [Var 2, Apply [Apply [Var 3, Var 4], Lit "10"]]], Apply [Var 2, Apply [Apply [Var 5, Var 4], Lit "10"]]]

-->

SApp (SFun 2 1) [SApp (SFun 1 2) [SApp (SFun 2 3) [SRef 4,SLit "10"]],SApp (SFun 1 2) [SApp (SFun 2 5) [SRef 4,SLit "10"]]]

-}
