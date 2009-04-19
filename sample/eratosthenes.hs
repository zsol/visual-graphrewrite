data List a = Nil | Cons a (List a)
              deriving (Show)

--result :: String
result = showIntList primes

not True  = False
not False = True

showInt 0 = "0"
showInt 1 = "1"
showInt 2 = "2"
showInt 3 = "3"
showInt 4 = "4"
showInt 5 = "5"
showInt 6 = "6"
showInt 7 = "7"
showInt 8 = "8"
showInt 9 = "9"
showInt n = (++) (showInt (div n 10)) (showInt (mod n 10))

showIntList Nil = ""
showIntList (Cons h t) = (++) (showInt h) ((++) ", " (showIntList t))

--primesbelow :: Int -> String
--primesbelow n = showIntList (eratos (enumFT 2 n))

--f :: Int -> Int -> Bool
f h x = not ((eqInt) (mod x h) 0)

--eratos :: [Int] -> [Int]
eratos (Cons h t) = Cons h (fil (f h) (eratos t))
eratos Nil = Nil

--enumFT :: Int -> Int -> [Int]
--enumFT f t
--    | f > t = Nil
--    | otherwise = Cons f (enumFT ((+) f 1) t)

--enumF :: Int -> [Int]
enumF f = Cons f (enumF (succ f))

--primes :: [Int]
primes = eratos (enumF 2)

--fil :: (a -> Bool) -> [a] -> [a]
fil v Nil = Nil                       -- @ (@ fil v) Nil
fil p l@(Cons h t) = fil' p l (p h)

{-
type Graph = (Int, IntMap X)

type FunMap = IntMap Rule

type Fun = [(Graph, Graph)]

fil = [(g1, g2), (g3, g4)]

g1 = (0, fromList [(0,App (Ref 1) (Ref 2)),(1,App (Fun `fil`) (Ref 3)),(2,Con Nil),(3,Var 0)])
g2 = (0, fromList [(0,Con Nil)])
g3 = (0, fromList [(0,App (Ref 1) (Ref 2)),(1,App (Fun `fil`) (Ref 3)),(2,App ),(3,W)
g4 = (0, fromList   [(0, App (Ref 1) (Ref 5)
                    ,(1, App (Ref 2) (Ref 4)
                    ,(2, App (Fun `fil`) (Ref 3)
                    ,(3, Var 3)
                    ,(4, Var 2)


-}

fil' p (Cons h t) True = Cons h (fil p t)
fil' p (Cons h t) False = fil p t


{-
eratos [2,3,4,5,6,7]
== 2:filter (f 2) (eratos [3,4,5,6,7])
== 2:filter (f 2) (3:filter (f 3) (eratos [4,5,6,7]))
== 2:filter (f 2) (3:filter (f 3) (4:filter (f 4) (eratos [5,6,7])))
== 2:filter (f 2) (3:filter (f 3) (4:filter (f 4) (5:filter (f 5) (eratos [6,7]))))
== 2:filter (f 2) (3:filter (f 3) (4:filter (f 4) (5:filter (f 5) (6:filter (f 6) (eratos [7])))))
== 2:filter (f 2) (3:filter (f 3) (4:filter (f 4) (5:filter (f 5) (6:filter (f 6) (7:filter (f 7) (eratos []))))))
== 2:filter (f 2) (3:filter (f 3) (4:filter (f 4) (5:filter (f 5) (6:filter (f 6) (7:filter (f 7) [])))))
== 2:filter (f 2) (3:filter (f 3) (4:filter (f 4) (5:filter (f 5) (6:filter (f 6) (7:[])))))
== 2:filter (f 2) (3:filter (f 3) (4:filter (f 4) (5:filter (f 5) (6:7:[]))))
== 2:filter (f 2) (3:filter (f 3) (4:filter (f 4) (5:6:7:[])))
== 2:filter (f 2) (3:filter (f 3) (4:5:6:7:[]))
== 2:filter (f 2) (3:4:5:7:[])
== 2:3:5:7:[]
-}
