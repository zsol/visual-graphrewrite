{- |
   This is our representation of a Haskell module.
-}
module SimpleHaskell
where
  -- | A Simple module consists of zero or more declarations.
  type SimpModule a = [Decl a]

  -- | A declaration is either a function binding or a pattern binding.
  data Decl a
      = FunBind [FunAlt a] -- ^ A function binding consists of one or more function alternatives.
      | PatBind a (Expr a) -- ^ A pattern binding consists of a pattern and an expression which is bound to the pattern.
        deriving (Show, Eq)

  -- | A function alternative is made up by the name of the function, the list of arguments and the expression.
  type FunAlt a = (a, [a], Expr a) 

  -- | Four things are considered an expression.
  data Expr a 
      = Let [Decl a] (Expr a) -- ^ A let expression consists of a list of declarations which's scope is limited to the following expression.
      | Var a -- ^ This is a simple variable identified by the type 'a'.
      | Apply [Expr a] -- ^ Expressions can be applied to each other with this constructor.
      | Lit String -- ^ This is a simple string literal.
	deriving (Show, Eq)

