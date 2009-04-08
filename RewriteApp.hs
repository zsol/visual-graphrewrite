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



