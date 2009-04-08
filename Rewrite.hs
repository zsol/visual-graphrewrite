
module Rewrite
    ( rewriteHNF )
where
  import Prelude hiding (exp)

  import qualified Data.IntMap as I
  import qualified Data.List as L

  import RewriteAppTypes
  import qualified SimpleHaskell as SH

  import Data.Maybe

  data Result = OK | Fail

-------------------- eliminate SRef

  deref :: Expr -> I.IntMap Expr -> Expr
  deref (SRef ref) im = case I.lookup ref im of
                          Just e  -> deref e im
                          Nothing -> error "deref" -- SRef ref
  deref e _ = e

-------------------- flatten SApp in firts arguments

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

  rewriteHNF' :: RewriteSystem -> Graph -> Expr -> PointedGraph
  rewriteHNF' rs g e = fromMaybe (e, g) $ rewriteHNF rs g e 

  rewriteHNF :: RewriteSystem -> Graph -> Expr -> Maybe PointedGraph
  rewriteHNF rs g e = 
      case flattenSApp (deref e g) g of
        (SFun ar f, l) 
            | length l == ar -> firstMatch rs g l rules
            | length l  > ar -> do
                      (e, g) <- firstMatch rs g (take ar l) rules
                      pg <- rewriteHNF rs g (SApp e (drop ar l))
                      return pg
            | otherwise      -> Nothing
            where
                rules = fromMaybe (error "nincs meg a szabály") (I.lookup f rs)
{-
Itt bele kellene tenni, hogy ha nincs meg a szabaly, akkor a fuggveny egy "delta" fuggveny. Ekkor a kovetkezot kell tenni:
 - a fuggveny osszes argumentumat redukalni. Ezek vegen literalokat kell kapnunk ha tipushelyes a program (a delta fuggvenyek csak elemi tipusokhoz elemi tipusokat rendelhetnek.
 - mintaillesztest vegezni a fuggvenyre
   - ha sin, akkor szamolni a literal szinuszat
   - ha cos, akkor szamolni a literal koszinuszat
   - ...
 - a vegeredmeny literalra atiranyitani azokat az eleket, amik a delta fuggvenyre mutattak 
-}
        _ -> Nothing


  firstMatch :: RewriteSystem -> Graph -> [Expr] -> [Rule] -> Maybe PointedGraph
  firstMatch _ _ _ [] = Nothing
  firstMatch rs g es (rule:rules)
     = case matches rs g es (patts rule) I.empty  of
         (g, Just bs) -> Just (substitute bs (exp rule), g)
         _            -> firstMatch rs g es rules

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
  match rs g e (SHole n) bs = (g, Just (I.insert n e bs))
  match rs g e (SLit y)  bs
        = case rewriteHNF' rs g e of
            (SLit x, g)  | x == y      -> (g, Just bs)
            _                          -> (g, Nothing)
  match rs g e (SCons y) bs
        = case rewriteHNF' rs g e of
            (SCons x, g)  | x == y     -> (g, Just bs)
            _                          -> (g, Nothing)
{-
  match rs g e (SApp y ys) bs
        = case rewriteHNF' rs g e of
            (SApp x xs, Just bs)       -> matches rs g (x:xs) (y:ys) bs
            _                          -> (g, Nothing)
-}
                                      
{-

Apply [Apply [Var "++", Apply [Var "showInt", Apply [Apply [Var "div", Var "n"], Lit "10"]]],
       Apply [Var "showInt", Apply [Apply [Var "mod", Var "n"], Lit "10"]]]

( 1 -> "++", 2 -> "showInt", 3 -> "div", 4 -> "n", 5 -> "mod" )

-->

Apply [Apply [Var 1, Apply [Var 2, Apply [Apply [Var 3, Var 4], Lit "10"]]], Apply [Var 2, Apply [Apply [Var 5, Var 4], Lit "10"]]]

-->

SApp (SFun 2 1) [SApp (SFun 1 2) [SApp (SFun 2 3) [SRef 4,SLit "10"]],SApp (SFun 1 2) [SApp (SFun 2 5) [SRef 4,SLit "10"]]]

-}
