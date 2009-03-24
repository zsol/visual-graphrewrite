module RewriteApp
where
  import Prelude hiding (exp)

  import qualified Data.IntMap as I
  import qualified Data.List as L

  import RewriteAppTypes
  import qualified SimpleHaskell as SH

  data Result = OK | Fail

  makeRewriteRules :: SH.SimpModule Int -> RewriteSystem
  makeRewriteRules []    = I.empty
  makeRewriteRules (h:t) = case I.lookup hid rest of
                             Just rs -> I.insert hid (extend rs h) rest
                             Nothing -> I.insert hid (extend [] h) rest
      where
        hid  = SH.name' h
        rest = makeRewriteRules t
        extend l (SH.FunBind as)  = L.nub $ l ++ map makeRule as
        extend l _ = l

  makeRule :: SH.FunAlt Int -> Rule
  makeRule (f, ps, e) = Rule 
                        { patts = map makeExprLeft ps
                        , RewriteAppTypes.exp   = makeExprRight e
                        , graph = I.empty -- TODO
                        }

  makeExprLeft :: SH.Expr Int -> Expr
  makeExprLeft (SH.Var v) = SHole v
  makeExprLeft (SH.Cons c) = SCons c
  makeExprLeft (SH.Apply (h@(SH.Cons c):t)) = SApp (SCons c) (map makeExprLeft t)
  makeExprLeft (SH.Lit l) = SLit l

  makeExprRight :: SH.Expr Int -> Expr
  makeExprRight (SH.Var v) = SRef v
  makeExprRight (SH.Cons c) = SCons c
  makeExprRight (SH.Lit l) = SLit l
  makeExprRight (SH.Apply (h@(SH.Cons c):t)) = SApp (SCons c) (map makeExprRight t)
  makeExprRight (SH.Apply (h@(SH.Var v):t))  = SApp (SFun (length t) v) (map makeExprRight t) -- ez vajon igy jo?
  makeExprRight (SH.Apply (h@(SH.Apply (ih:it)):t)) = SApp (fst la) (snd la)
      where
        la = liftApp $ makeExprRight h
        liftApp (SApp (SFun a f) l) = (SFun (a + (length t)) f, l ++ (map makeExprRight t))
        liftApp (SApp (SCons c) l) = (SCons c, l ++ (map makeExprRight t))


  deref :: Expr -> I.IntMap Expr -> Expr
  deref (SRef ref) im = case I.lookup ref im of
                          Just e  -> e
                          Nothing -> SRef ref
  deref e _ = e
 
  rewriteHNF :: RewriteSystem -> Graph -> Maybe Graph
  rewriteHNF rs (e, g) = 
      case deref e g of
        SApp x l -> case deref x g of
                     SFun ar f 
                       | length l >= ar -> firstMatch' (I.lookup f rs)
                       where
                         (l1, l2) = splitAt ar l
                         firstMatch' Nothing = Nothing
                         firstMatch' (Just x) = firstMatch x
                         firstMatch [] = Nothing
                         firstMatch (rule:rules)
                             = case L.mapAccumL (match rs) (g, I.fromList []) (zip (patts rule) l1) of
                                 ((g', binds), s) | or s == False -> firstMatch rules
                                                  | otherwise     -> Just (exp rule, g')
                     _ -> Nothing
        SFun ar _ | ar == 0 -> Nothing
        _ -> Nothing
                  
  match :: RewriteSystem -> (I.IntMap Expr, I.IntMap Expr) -> (Expr, Expr) -> ((I.IntMap Expr, I.IntMap Expr), Bool)
  match rs (g, binds) (SHole n, y)   = ((g, I.insert n y binds), True)
  match rs gb (SLit x, SLit y)
      | x == y                       = (gb, True)
      | otherwise                    = (gb, False)
  match rs gb (SCons x, SCons y)
      | x == y                       = (gb, True)
      | otherwise                    = (gb, False)
  match rs gb (SApp x xs, SApp y ys) = (a, and bs)
      where
        (a, bs) = L.mapAccumL (match rs) gb ((x,y) : zip xs ys)
  match rs (g, binds) (x, y)         = case rewriteHNF rs (y, g) of
                                         Just (y', g') -> match rs (g', binds) (x, y')
                                         Nothing       -> ((g, binds), False)

                                      
{-

Apply [Apply [Var "++", Apply [Var "showInt", Apply [Apply [Var "div", Var "n"], Lit "10"]]],
       Apply [Var "showInt", Apply [Apply [Var "mod", Var "n"], Lit "10"]]]

( 1 -> "++", 2 -> "showInt", 3 -> "div", 4 -> "n", 5 -> "mod" )

-->

Apply [Apply [Var 1, Apply [Var 2, Apply [Apply [Var 3, Var 4], Lit "10"]]], Apply [Var 2, Apply [Apply [Var 5, Var 4], Lit "10"]]]

-->

SApp (SFun 2 1) [SApp (SFun 1 2) [SApp (SFun 2 3) [SRef 4,SLit "10"]],SApp (SFun 1 2) [SApp (SFun 2 5) [SRef 4,SLit "10"]]]

-}