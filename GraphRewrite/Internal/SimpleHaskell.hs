{- |
   This is our representation of a Haskell module.
-}
module GraphRewrite.Internal.SimpleHaskell
where
  -- | A Simple module consists of zero or more declarations.
  type SimpModule a = [Decl a]

  -- | A declaration is either a function binding or a pattern binding.
  data Decl a
      = FunBind [FunAlt a] -- ^ A function binding consists of one or more function alternatives.
      | PatBind (Patt a) (Expr a) -- ^ A pattern binding consists of a pattern and an expression which is bound to the pattern.
--      | DataDecl a
        deriving (Show, Eq)

  -- | A function alternative is made up by the name of the function, the list of arguments and the expression.
  type FunAlt a = (a, [Patt a], Expr a)

  -- | These things are considered an expression.
  data Expr a
      = Let [Decl a] (Expr a) -- ^ A let expression consists of a list of declarations which's scope is limited to the following expression.
      | Var a -- ^ This is a simple variable identified by the type 'a'.
      | Cons a  -- ^ e.g: Left, Just, Nothing
      | Apply [Expr a] -- ^ Expressions can be applied to each other with this constructor.
      | Lit String -- ^ This is a simple string literal.
      | AsPat a (Patt a) -- ^ This is an alias pattern represented by '@' in Haskell. Should only be inside a Pattern.
        deriving (Show, Eq)

  -- | Convenience alias for an expression
  type SExpr = Expr

  -- | A pattern is essentially an expression with the exception that it can not be a Let constructor.
  type Patt = Expr

  -- | This function gets the name of a declaration. This is the name of the entity that we declare.
  name :: Decl String -> String
  name (PatBind n  _) = head $nameExpr n
  name (FunBind ((x,_,_):_)) = x
--  name (DataDecl a) = a

  -- | Gets the name of an expression.
  nameExpr :: Expr String -> [String]
  nameExpr (Var n) = [n]
  nameExpr (Cons n) = [n]
  nameExpr (Lit _n) = []
  nameExpr (AsPat p e) = p : nameExpr e
  nameExpr (Apply es) = concatMap nameExpr es

  name' :: Decl a -> a
  name' (FunBind ((x,_,_):_)) = x
  name' (PatBind n _) = head $ nameExpr' n

  nameExpr' :: Expr a -> [a]
  nameExpr' (Var n) = [n]
  nameExpr' (Cons n) = [n]
  nameExpr' (Apply es) = concatMap nameExpr' es



{-
  data Patt a
     = Cons a [Patt a]
     | PVar a -- ^ This is a simple variable identified by the type 'a'.
     | PLit String -- ^ This is a simple string literal.
-}


