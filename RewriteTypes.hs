-- | Types which are specific to a graph rewrite system.
module RewriteTypes
where

  import Data.IntMap

  type Arity = Int -- ^ Arity is a non-negative integer which represents the number of arguments a function can take.

  -- | An expression.
  data Expr
      = SCons Int        -- ^ A constructor token. May occur on either the left or right side of a rule.
      | SFun  Arity Int  -- ^ A function token. May only occur on the right side of a rule (definition).
      | SLit String      -- ^ String literal. Usually handled the same way as a 0 argument constructor.
      | SHole Int        -- ^ Represents join points in a rule. Should not appear in data graphs.
      | SRef Int         -- ^ Refers to an other expressions. Sharing can be expressed with this token in data graphs and right side of rules.
      | SApp Expr [Expr] -- ^ Represents an application. An expression can be applied to a list of expressions. The first one can only be SFun or SCons (or SRef). Can appear everywhere, but if on the right side of a rule, the first expression can only be an SCons.
        deriving (Eq, Show)

  -- | A rewrite system is essentially a mapping of function identifiers to alternative definitions.
  data RewriteSystem = RewriteSystem
      { rules :: IntMap [Rule]
      , names :: IntMap String -- TODO: document this.
      }
                       deriving (Show)

  defaultRS = RewriteSystem { rules = empty, names = empty }


  -- | A rule represents a function alternative.
  data Rule = Rule
    { patts :: [Expr]     -- ^ A list of expressions representing pattern bindings.
    , exp   :: Expr       -- ^ The function definition.
    , graph :: Graph      -- ^ Images of references in the definition.
    }
              deriving (Eq, Show)

  -- | A graph is represented by a mapping from integers to expressions.
  type Graph = IntMap Expr

  -- | This is a normal graph with one expression designated as root node.
  type PointedGraph = (Expr, Graph)

{-
 f x = x

 Rule
    { patts = [SHole 3]
    , exp   = SHole 3
    , graph = fromList []
    }

------------------------------------------

 f x = y + y where y = x * x

 Rule
    { patts = [SHole 3]
    , exp   = SApp (SFun 2 320) [SRef 0, SRef 0]
    , graph = fromList
        [ (0, SApp (SFun 2 321) [SHole 3, SHole 3])
        ]

----------------------------------------- lehet hogy régi
egy szabály:


  (++) ((:) x xs) ys   =  (:) x ((++) xs ys)
  (++) _          ys   =  ys

 -->

  (++) ((:) 4@x 5@xs) 2@ys   =  (:) x ((++) xs ys)
  (++) 1@_          2@ys   =  ys

 -->

  Rules (++) [r1, r2]

  r1 = Rule
            [ SApp (SCons (:)) [SHole 4, SHole 5]
            , SHole 2]
            (SApp (SCons (:)) [SHole 4, SApp (SFun (++) [SHole 5, SHole 2])])

  r2 = Rule [ SHole 1, SHole 2] (SHole 2)
----------------------------------------
  app 1@f 2@x = 3@f x

-->
  Rules "app" [Rule [SHole 1, SHole 2] (SApp (SFun "f") [SHole 2])]

----------------------------------------

  f 1@x@((:) 2@a 3@b) = 4@(++) x b

-->
  Rules "f"
    Rule [SRef 1]
         (SApp (SFun (++)) [SRef 1, SHole 3])
         [ 1 |-> Rule [SApp (SCons (:)) [SHole 2, SHole 3]]
----------------------------------------

  result = 1@(f 2@1)

-->
  Rules "result" [Rule [] 1 [1 |-> (SFun "f", [2]), 2 |-> (SLit "1", [])

----------------------------------------

    result = f y

    y = sum [1..10000]

    f x = x + x + y
-->

    result = f y y
      where
        y = 10

    f y x = x + x + y

---------------------------------------

    cycle 1@x = 2@y  where  y = x ++ y

-->
   Rules "cycle" [
        Rule [SHole 1]
             (SRef 1)
             [ 1 -> SApp (SFun (++)) [SHole 1, SRef 1 ]]
-}

