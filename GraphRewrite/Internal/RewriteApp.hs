
module GraphRewrite.Internal.RewriteApp
    ( makeRewriteRules, invMakeExpr, makeRewriteSystem )
where
  import Prelude hiding (exp)

  import qualified Data.IntMap as I
  import qualified Data.List as L

  import GraphRewrite.Internal.RewriteTypes
  import qualified GraphRewrite.Internal.SimpleHaskell as SH

  makeRewriteSystem :: SH.SimpModule Int -> I.IntMap String -> RewriteSystem
  makeRewriteSystem m nms = (makeRewriteRules m) { names = nms }

  makeRewriteRules :: SH.SimpModule Int -> RewriteSystem
  makeRewriteRules []    = defaultRS
  makeRewriteRules (h:t) = case I.lookup hid (rules rest) of -- FIXME vv
                             Just rs -> defaultRS { rules = I.insert hid (extend rs h) (rules rest) }
                             Nothing -> defaultRS { rules = I.insert hid (extend [] h) (rules rest) }
      where
        hid  = SH.name' h
        rest = makeRewriteRules t
        extend l (SH.FunBind as)  = L.nub $ l ++ map makeRule as
        extend l (SH.PatBind (SH.Var v) e) = extend l (SH.FunBind [(v, [], e)]) -- special case to handle variable bindings
        extend l _ = l

  makeRule :: SH.FunAlt Int -> Rule
  makeRule (_, ps, e) = fixExpr Rule { patts = map makePat ps,
                                       exp = makeRefExpr e,
                                       graph = makeGraph (e:ps) }

  makePat :: SH.Expr Int -> Expr
  makePat (SH.AsPat _ e) = makeExpr e
  makePat x = makeExpr x

  makeExpr :: SH.Expr Int -> Expr
  makeExpr (SH.Var v) = SHole v
  makeExpr (SH.Cons c) = SCons c
  makeExpr (SH.Lit l) = SLit l
  makeExpr (SH.Let _ e) = makeRefExpr e
  makeExpr (SH.Apply ((SH.Lit l):t))  = SApp (SLit l)  (map makeExpr t) -- this really shouldn't be used.
  makeExpr (SH.Apply ((SH.Cons c):t)) = SApp (SCons c) (map makeExpr t)
  makeExpr (SH.Apply ((SH.Var v):t))  = SApp (SFun (length t) v) (map makeExpr t)
  makeExpr (SH.Apply (h@(SH.Apply (_:_)):t)) = uncurry SApp la
      where
        la = liftApp $ makeExpr h
        liftApp (SApp (SFun a f) l) = (SFun (a + length t) f, l ++ map makeExpr t)
        liftApp (SApp (SCons c) l) = (SCons c, l ++ map makeExpr t)

  makeRefExpr :: SH.Expr Int -> Expr
  makeRefExpr = holeToRef . makeExpr
      where
        holeToRef (SHole h) = SRef h
        holeToRef (SApp e es) = SApp (holeToRef e) (map holeToRef es)
        holeToRef e = e

  fixExpr :: Rule -> Rule
  fixExpr r@(Rule {exp = e, graph = g}) = r { exp = doFix e }
      where
        doFix x@(SHole h) = case I.lookup h g of
                              Just _ -> SRef h
                              _      -> x
        doFix (SApp e es) = SApp (doFix e) (map doFix es)
        doFix x = x



  makeGraph :: [SH.Expr Int] -> Graph
  makeGraph (p:ps) = case p of
                       SH.AsPat a e -> I.insert a (makeExpr e) (makeGraph ps)
                       SH.Let ((SH.PatBind p pe):ds) e -> let patid = read (exprID $ makePat p) :: Int in
                                                         I.insert patid (makeRefExpr pe) (makeGraph (SH.Let ds e : ps))
                       SH.Let [] _ -> makeGraph ps --redundant
                       SH.Let ((SH.FunBind _):_) _ -> error "makeGraph error: FunBinds unsupported in Let"
                       _           -> makeGraph ps

  makeGraph [] = I.empty

  invMakeExpr :: Expr -> SH.Expr Int
  invMakeExpr (SCons c) = SH.Cons c
  invMakeExpr (SLit l)  = SH.Lit l
  invMakeExpr (SFun _ f) = SH.Var f
  invMakeExpr (SHole v) = SH.Lit ("THIS IS A BUG - " ++ show v)
  invMakeExpr (SApp x xs) = SH.Apply (invMakeExpr x : map invMakeExpr xs)

