-- | This module contains functions which perform the graph rewriting procedure.
module GraphRewrite.Internal.Rewrite
{-    ( rewriteHNF
    , rewriteStep
    , rewriteStep'
    , rewriteSteps
    , rewriteStepFine
    )-} where

  import GraphRewrite.Internal.RewriteTypes
  import GraphRewrite.Internal.DeltaFunctions

  import qualified Data.IntMap as I

  import Data.Maybe
  import Prelude hiding (exp)

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

  rewriteSteps :: RewriteSystem -> Expr -> Graph -> [PointedGraph]
  rewriteSteps rs e g = case rewriteStep rs e g of
                          Nothing      -> []
                          Just p@(e,g) -> p : rewriteSteps rs e g

  rewriteStepsFine :: RewriteSystem -> Expr -> Graph -> RewriteTree
  rewriteStepsFine rs e g = Step (e,g) [rewriteStepFine rs e g]

  -- | Does the rewriting on a specified expression and returns detailed results.
  rewriteStepFine
      :: RewriteSystem -- ^ The context.
      -> Expr           -- ^ Expression to be rewritten.
      -> Graph          -- ^ Graph showing images of possible references in the expression.
      -> RewriteTree    -- ^ Resulting tree which describes the rewriting process in great detail.
  rewriteStepFine rs e g =
      case e of
        SRef _             -> let ref = deref' rs e g
                             in Step (ref, g) [rewriteStepFine rs ref g]
        SApp (SApp _ _) _  -> let
                                 (flatExpr, flatArgs) = flattenSApp rs (deref rs e g) g
                                 flatApp              = SApp flatExpr flatArgs
                             in Step (flatApp,g) [rewriteStepFine rs flatApp g]
        SApp (SFun ar f) l -> funInApp f ar l
        _                  -> Step (e,g) []
      where
        funInApp f ari args = case I.lookup f (rules rs) of
                                Just rls
                                    | length args == ari ->
                                        case firstMatchFine rs g args rls of
                                          Just ((e',g'), trees) -> Step (e,g) (trees ++ [rewriteStepFine rs e' g'])
                                          Nothing    -> Step (e,g) []
                                    | length args >  ari -> case firstMatchFine rs g (take ari args) rls of --TODO: do this properly
                                                             Just ((e,g), trees) -> Step (e,g) (trees ++ [rewriteStepFine rs e g])
                                                             Nothing    -> Step (e,g) []
                                    | otherwise          -> Step (e,g) []
                                Nothing -- no function definition found -> probably a delta function
                                    -> let
                                          steps = map rewriteExpFine args
                                          fname = fromMaybe (error $ "No name found for function: " ++ show f) (I.lookup f (names rs))
                                          delta = fromMaybe (error $ "Cannot rewrite delta: " ++ fname) (rewriteDelta fname (map (fst . lastGraph) steps))
                                      in Step (e,g) (steps ++ [rewriteStepFine rs delta I.empty])
        rewriteExpFine = flip (rewriteStepFine rs) g




{-
  rewriteStepFine
      :: RewriteSystem -- ^ A rewrite system which contains rules
      -> Expr          -- ^ Expression to be rewritten
      -> Graph         -- ^ Graph showing images of references
      -> [PointedGraph] -- ^ Just the resulting pointed graph or Nothing if rewriting is impossible.
  --visszaadja az átírási lépéseket addig, amíg rewriteStep, de finomabban

  rewriteStepFine rs e g = case rewriteStep' rs e g of
     [] -> Nothing
     l  -> last l
-}

  -- | Does a rewrite step on the specified expression maybe returning the result.
  rewriteStep
      :: RewriteSystem -- ^ A rewrite system which contains rules
      -> Expr          -- ^ Expression to be rewritten
      -> Graph         -- ^ Graph showing images of references
      -> Maybe PointedGraph -- ^ Just the resulting pointed graph or Nothing if rewriting is impossible.
  rewriteStep rs e g =
      case flattenSApp rs (deref rs e g) g of
        (SFun ar f, l) -> case rls of
                           Just rls
                               | length l == ar -> firstMatch rs g l rls
                               | length l  > ar -> do
                                       (e, g) <- firstMatch rs g (take ar l) rls
                                       rewriteStep rs (SApp e (drop ar l)) g
                               | otherwise      -> Nothing
                           Nothing -> do
                                    let l' = map (fst . rewriteExp) l
                                    f' <- I.lookup f (names rs)
                                    e <- rewriteDelta f' l'
                                    -- a vegeredmeny literalra atiranyitani azokat az eleket, amik a delta fuggvenyre mutattak
                                    return (e, I.empty)
            where
                rls = (I.lookup f (rules rs))
                rewriteExp = flip (rewriteHNF rs) g

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
  substitute bs (SHole n) = substitute bs (SRef n) --TODO: not really sure about this
  substitute bs (SApp e es) = SApp (substitute bs e) (map (substitute bs) es)
  substitute _ e = e

  -- | Pattern matching for multiple expressions and patterns. See also 'match'.
  matches
    :: RewriteSystem
    -> Graph
    -> [Expr]         -- ^ Expressions
    -> [Expr]         -- ^ Patterns
    -> I.IntMap Expr  -- ^ Binds
    -> (Graph, Maybe (I.IntMap Expr))

  matches _ g [] [] bs = (g, Just bs)
  matches rs g (e:es) (p:ps) bs
    = case match rs g e p bs of
        (g, Just bs)    -> matches rs g es ps bs
        x               -> x

  -- | Does the pattern matching.
  match
    :: RewriteSystem
    -> Graph          -- ^ Images of references
    -> Expr           -- ^ Expression to be matched.
    -> Expr           -- ^ Pattern
    -> I.IntMap Expr  -- ^ Binds
    -> (Graph, Maybe (I.IntMap Expr))
  match _ g e (SHole n) bs = (g, Just (I.insert n e bs))
  match rs g e (SLit y)  bs
        = case rewriteStep' rs e g of
            (SLit x, g)  | x == y      -> (g, Just bs)
            _                          -> (g, Nothing)
  match rs g e (SCons y) bs
        = case rewriteStep' rs e g of
            (SCons x, g)  | x == y     -> (g, Just bs)
            _                          -> (g, Nothing)

  match rs g e (SApp y ys) _
        = case rewriteStep' rs e g of
            (SApp x xs, bs)            -> matches rs g (x:xs) (y:ys) bs
            _                          -> (g, Nothing)

  -- | Fine pattern matching. Returns the rewrite steps needed to match the expression against the pattern.
  matchFine
      :: RewriteSystem
      -> Graph
      -> Expr -- ^ Pattern to be matched against.
      -> Expr -- ^ Expression to be matched.
      -> I.IntMap Expr -- ^ Already existing matches (bindings).
      -> (Maybe (I.IntMap Expr), [RewriteTree]) -- ^ The first component is 'Nothing' if the match is unsuccessful, otherwise it contains the original bindings possibly extended (as a result of the current match). The second component is a list of extra rewriting steps required to resolve the expression to match the pattern.
  matchFine _ _ (SHole n) e binds = (Just (I.insert n e binds), [])
  matchFine rs g (SLit y) e binds
      = let tree = rewriteStepFine rs e g in
        case lastGraph tree of
          (SLit x, _)  | x == y -> (Just binds, [tree])
          _                     -> (Nothing, [])
  matchFine rs g (SCons y) e binds
      = let tree = rewriteStepFine rs e g in
        case lastGraph tree of
          (SCons x, _) | x == y -> (Just binds, [tree])
          _                     -> (Nothing, [])
  matchFine rs g (SApp y ys) e binds
      = let tree = rewriteStepFine rs e g in
        case lastGraph tree of
          (SApp x xs, _)        -> matchesFine rs g (y:ys) (x:xs) binds [tree]
          _                     -> (Nothing, [])

  -- | Fine pattern matching for multiple expressions and patterns. See also: 'matchFine'.
  matchesFine
      :: RewriteSystem
      -> Graph
      -> [Expr] -- ^ List of patterns.
      -> [Expr] -- ^ List of expressions.
      -> I.IntMap Expr -- ^ Bindings already bound.
      -> [RewriteTree] -- ^ Rewrite steps already needed.
      -> (Maybe (I.IntMap Expr), [RewriteTree]) -- ^ The first component is 'Nothing' if the match is unsuccessful, otherwise it contains the original bindings possibly extended (as a result of the current match). The second component is a list of extra rewriting steps required to resolve the expression to match the pattern.
  matchesFine _ _ [] [] binds trees = (Just binds, trees)
  matchesFine rs g (p:ps) (e:es) binds trees
      = case matchFine rs g p e binds of
          (Just binds, newtrees)  -> matchesFine rs g ps es binds (trees ++ newtrees)
          x@(Nothing, _)          -> x

  -- | Applies the first matching rule for a list of arguments. Also returns the rewrite steps needed for the underlying pattern matching.
  firstMatchFine
      :: RewriteSystem
      -> Graph
      -> [Expr] -- ^ Arguments.
      -> [Rule] -- ^ Possible rules.
      -> Maybe (PointedGraph, [RewriteTree]) -- ^ 'Nothing' if pattern matching is unsuccessful; otherwise the first component is a 'PointedGraph' on which the rule was already applied, the second component contains the rewrite steps needed for the pattern matching.
  firstMatchFine _ _ _ [] = Nothing
  firstMatchFine rs g exprs (rule:rules)
      = case matchesFine rs g (patts rule) exprs I.empty [] of
          (Just binds, trees) -> Just ((substitute binds (exp rule), g), trees)
          _                   -> firstMatchFine rs g exprs rules
{-

Apply [Apply [Var "++", Apply [Var "showInt", Apply [Apply [Var "div", Var "n"], Lit "10"]]],
       Apply [Var "showInt", Apply [Apply [Var "mod", Var "n"], Lit "10"]]]

( 1 -> "++", 2 -> "showInt", 3 -> "div", 4 -> "n", 5 -> "mod" )

-->

Apply [Apply [Var 1, Apply [Var 2, Apply [Apply [Var 3, Var 4], Lit "10"]]], Apply [Var 2, Apply [Apply [Var 5, Var 4], Lit "10"]]]

-->

SApp (SFun 2 1) [SApp (SFun 1 2) [SApp (SFun 2 3) [SRef 4,SLit "10"]],SApp (SFun 1 2) [SApp (SFun 2 5) [SRef 4,SLit "10"]]]

-}
