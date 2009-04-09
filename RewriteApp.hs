module RewriteApp 
    ( makeRewriteRules )
where
  import Prelude hiding (exp)

  import qualified Data.IntMap as I
  import qualified Data.List as L

  import RewriteAppTypes
  import qualified SimpleHaskell as SH

  makeRewriteRules :: SH.SimpModule Int -> RewriteSystem
  makeRewriteRules []    = I.empty
  makeRewriteRules (h:t) = case I.lookup hid rest of
                             Just rs -> I.insert hid (extend rs h) rest
                             Nothing -> I.insert hid (extend [] h) rest
      where
        hid  = SH.name' h
        rest = makeRewriteRules t
        extend l (SH.FunBind as)  = L.nub $ l ++ map makeRule as
        extend l (SH.PatBind (SH.Var v) e) = extend l (SH.FunBind [(v, [], e)]) -- special case to handle variable bindings
        extend l _ = l

  makeRule :: SH.FunAlt Int -> Rule
  makeRule (f, ps, e) = fixExpr $ Rule { patts = map makePat ps,
                                         exp = makeExpr e,
                                         graph = makeGraph ps } 

  makePat :: SH.Expr Int -> Expr
  makePat (SH.AsPat a e) = makeExpr e
  makePat x = makeExpr x

  makeExpr :: SH.Expr Int -> Expr
  makeExpr (SH.Var v) = SHole v
  makeExpr (SH.Cons c) = SCons c
  makeExpr (SH.Lit l) = SLit l
  makeExpr (SH.Apply (h@(SH.Cons c):t)) = SApp (SCons c) (map makeExpr t)
  makeExpr (SH.Apply (h@(SH.Var v):t))  = SApp (SFun (length t) v) (map makeExpr t) -- ez vajon igy jo?
  makeExpr (SH.Apply (h@(SH.Apply (ih:it)):t)) = SApp (fst la) (snd la)
      where
        la = liftApp $ makeExpr h
        liftApp (SApp (SFun a f) l) = (SFun (a + (length t)) f, l ++ (map makeExpr t))
        liftApp (SApp (SCons c) l) = (SCons c, l ++ (map makeExpr t))

  fixExpr :: Rule -> Rule
  fixExpr r@(Rule {exp = e, graph = g}) = r { exp = doFix e }
      where
        doFix x@(SHole h) = case I.lookup h g of 
                              Just e -> SRef h
                              _      -> x
        doFix (SApp e es) = SApp (doFix e) (map doFix es)
        doFix x = x
            
                                            
        
  makeGraph :: [SH.Expr Int] -> Graph
  makeGraph (p:ps) = case p of
                       SH.AsPat a e -> I.insert a (makeExpr e) (makeGraph ps)
                       otherwise    -> makeGraph ps
  makeGraph [] = I.empty

