
module Rewrite
    ( rewriteHNF )
where
  import Prelude hiding (exp)

  import qualified Data.IntMap as I
  import qualified Data.List as L

  import RewriteAppTypes
  import qualified SimpleHaskell as SH

  data Result = OK | Fail

-------------------- eliminate SRef

  deref :: Expr -> I.IntMap Expr -> Expr
  deref (SRef ref) im = case I.lookup ref im of
                          Just e  -> deref e im
                          Nothing -> error "deref" -- SRef ref
  deref e _ = e

-------------------- flatten SApp in firts arguments

  flattenSApp :: Expr -> Edges -> 
    ( Expr          -- csak SFun, SCons, SLit lehet
    , [Expr])       -- SFun estén lehet nem üres, SCons, SLit esetén üres
  flattenSApp (SApp x xs) g 
    = case deref x g of
      SApp y ys  -> flattenSApp (SApp y (ys ++ xs)) g
      x          -> (x, xs)
  flattenSApp x _       -- SLit, SCons, SFun  esetén
    = (x, [], [])


--------------------

  rewriteHNF' :: RewriteSystem -> Graph -> Expr -> PointedGraph
  rewriteHNF' rs g e = fromMaybe (e, g) $ rewriteHNF rs g e 

  rewriteHNF :: RewriteSystem -> Graph -> Expr -> Maybe PointedGraph
  rewriteHNF rs g e = 
      case flattenSApp (deref e g) g of
        (SFun ar f, l) 
            | length l == ar -> firstMatch rs g l rules
            | length l  > ar -> do
                      (e, g) <- firstMatch rs g (take ar l) rules
                      return $ rewriteHNF rs g (SApp e (drop ar l))
            | otherwise      -> Nothing
            where
                rules = fromMaybe (error "nincs meg a szabály") (I.lookup f rs)
        _ -> Nothing


  firstMatch :: RewriteSystem -> Graph -> [Expr] -> [Rule] -> Maybe PointedGraph
  firstMatch rs [] = Nothing
  firstMatch rs (rule:rules)
     = case matches rs g l1 (patts rule)  of
         ((g', binds), s) | or s == False -> firstMatch rs rules
                          | otherwise     -> Just (exp rule, g')


  matches 
    :: RewriteSystem 
    -> Graph 
    -> [Expr]         -- ^ kifejezéses
    -> [Expr]         -- ^ minták
    -> I.IntMap Expr  -- ^ kötések
    -> (Graph, Maybe (I.IntMap Expr))

  matches rs g [] [] bs = (g, Just bs)
  matches rs g (e:es) (p:ps) bs
    = case match rs g e p bs of 
        (g, Just bs)    -> matches rs g es ps bs
        x               -> x

                  
  match 
    :: RewriteSystem 
    -> Graph 
    -> Expr           -- ^ kifejezés
    -> Expr           -- ^ minta
    -> I.IntMap Expr  -- ^ kötések
    -> (Graph, Maybe (I.IntMap Expr))
  match rs g e (SHole n) bs = (g, Just (I.insert n y bs))
  match rs g e (SLit y)  bs
        = case rewriteHNF' rs g e of
            (SLit x, g)  | x == y      -> (g, Just bs)
            _                          -> (g, Nothing)
  match rs g e (SCons y) bs
        = case rewriteHNF' rs g e of
            (SCons x, g)  | x == y     -> (g, Just bs)
            _                          -> (g, Nothing)

  match rs g e (SApp y ys) bs
        = case rewriteHNF' rs g e bs of
            (SApp x xs, Just bs)       -> matches rs g (x:xs) (y:ys) bs
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
