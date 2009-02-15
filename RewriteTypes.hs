module RewriteTypes
where

  data Symbol 
      = SCons Int
      | SFun  Int
 -- ezeknek nincsenek gyerekeik
      | SLit String     -- konstruktor
      | SHole  -- Int
      | SApp            -- alkalmazás

  type NodeId = Int

  type RewriteSystem = [Rules]

  -- | A declaration is either a function binding or a pattern binding.
  data Rules 
      = Rules 
            Int     -- ^ Function id
            [Rule]  -- ^ alternatives

  data Rule = Rule 
    { patts :: [NodeId]
    , exp :: NodeId 
    , graph :: IntMap (Symbol, [NodeId])
    }


{-
egy szabály:


  (++) ((:) x xs) ys   =  (:) x ((++) xs ys)
  (++) _          ys   =  ys

 -->

  (++) 1@((:) 4@x 5@xs) 2@ys   =  3@(:) x 6@((++) xs ys)
  (++) 1@_          2@ys   =  ys

 -->

  Rules (++) [r1, r2]

  r1 = Rule [1, 2] 3 
            [ 1 |-> (SCons (:), [4,5])
            , 2 |-> (SHole, [])
            , 3 |-> (SCons (:), [4,6])
            , 4 |-> (SHole, [])
            , 5 |-> (SHole, [])
            , 6 |-> (SFun (++), [5,2])
            ]

  r2 = Rule [1, 2] 2
            [ 1 |-> (SHole, [])
            , 2 |-> (SHole, [])
            ]
----------------------------------------
  app 1@f 2@x = 3@f x

--> 
  Rules "app" [Rule [1, 2] 3  [1 |-> SHole, 2 |-> SHole, 3 |-> (SApp, [1, 2])]]

----------------------------------------

  f 1@x@((:) 2@a 3@b) = 4@(++) x b

--> 
  Rules "f" [Rule [1] 4  [1 |-> (SCons (:) [2, 3], 2 |-> SHole, 3 |-> SHole, 4 |-> (SFun (++), [1, 3])]]

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


-}
