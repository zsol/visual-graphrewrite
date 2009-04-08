module RewriteAppTypes
where

  import Data.IntMap
  
  type Arity = Int

  data Expr
      = SCons Int
      | SFun  Arity Int  -- mintában (szabály bal oldalán) nem lehet
 -- ezeknek nincsenek gyerekeik
      | SLit String      -- konstruktor
      | SHole Int        -- csak szabályban lehet (adatgráfban nem lehet)
      | SRef Int         -- adatgráfban és a szabályok jobb oldalán a megosztást (sharing) lehet vele kifejezni)
      | SApp Expr [Expr] -- alkalmazás, az első kifejezés csak SFun és SCons (vagy SRef lehet), mintákban csak SCons lehet
        deriving (Eq, Show)

  type RewriteSystem = IntMap [Rule]  -- ^ funids to alternatives

  data Rule = Rule 
    { patts :: [Expr]
    , exp   :: Expr
    , graph :: Graph      -- ^ images of references
    }
              deriving (Eq, Show)

  type Graph = IntMap Expr  

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

