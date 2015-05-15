module TLTree where

import Cp
import Data.Monoid

type Point = (Int,Int)
type Side = Int
type Tri  =(Point,Side)

data TLTree a = L a | N (TLTree a,(TLTree a,TLTree a)) deriving (Eq,Show)

inTLTree = either (L) (N)

outLTree :: TLTree a-> Either  a (TLTree a,(TLTree a,TLTree a))
outLTree (L a)    = i1 a
outLTree (N (t1,(t2,t3)) )= i2 (t1,(t2,t3))

-- (2) Ana + cata + hylo -------------------------------------------------------


recTLTree f = id -|-  (f ><(f><f))

cataTLTree a = a . (recTLTree (cataTLTree a)) . outLTree

anaTLTree f = inTLTree . (recTLTree (anaTLTree f) ) . f

hyloTLTree a c = cataTLTree a . anaTLTree c

baseTLTree g f = g -|- ( f >< (f >< f))

instance Functor TLTree
         where fmap f = cataTLTree ( inTLTree . baseTLTree f id )

-- tabalho a realizar 5.2.2

tipsTLTree :: TLTree b -> [b]
tipsTLTree = cataTLTree (either singl conc)
        where conc(l,(r,t))= l ++ r ++ t

countTLTree :: TLTree b -> Int
countTLTree = cataTLTree (either (const 1) ((uncurry (+)).(id >< (uncurry (+)) ) ) )


depthTLTree :: TLTree b -> Int
depthTLTree = cataTLTree (either (const 1) (succ.(uncurry max).(id >< (uncurry max))))

invTLTree :: TLTree b -> TLTree b
invTLTree = cataTLTree (inTLTree . (id -|- (\(a,(b,c))->(c,(b,a)))))

geraSierp  = anaTLTree
