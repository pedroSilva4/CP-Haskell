module Piramide where


import Cp

type PointT = (Int,Int,Int)
--z é necessário para a transformação das piramide interiores

type SideT= Int
--SideT vai ser a altura da piramide
type TriP = (PointT,SideT)


data PLTree a  = P TriP | C (PLTree a,((PLTree a,PLTree a),(PLTree a,PLTree a)))

to3D :: (((Int,Int),Int),Int) -> (TriP,Int)
to3D (((a,b),c),d) = (((a,b,0),c),d)


inPLTree  = either P C

outPLTree (P a)    = i1 a
outPLTree (C (t1,((t2,t3),(t4,t5))) )= i2 (t1,((t2,t3),(t4,t5)))

basePLTree g f = g -|- ( f >< ((f><f) >< (f><f)))

recPLTree f = id -|- ( f >< ((f><f) >< (f><f)))


cataPLTree a = a . (recPLTree (cataPLTree a)) . outPLTree



anaPLTree f = inPLTree . (recPLTree (anaPLTree f) ) . f



hyloPLTree a c = cataPLTree a . anaPLTree c


tipsPLTree = cataPLTree (either singl conc)
        where conc(p,((l1,l2),(r1,r2)))= p ++ l1 ++ l2 ++ r1 ++r2



geraPiramideSierp :: (TriP,Int) -> Either TriP ( (TriP,Int), (((TriP,Int),(TriP,Int)) , ((TriP,Int),(TriP,Int))) )
geraPiramideSierp (t,0) = i1 t
geraPiramideSierp (t,a) = i2 (( inPup t, a-1) , ( ((leftP t,a-1) ,(rightP t,a-1)) , ((frontP t,a-1),(backP t,a-1)) ))


apresentaPiramide :: PLTree TriP -> [TriP]
apresentaPiramide = tipsPLTree

inPup :: TriP -> TriP
inPup ((a,b,c),d) = ((a,b+size,c),size)
                        where size = div d 2

leftP :: TriP -> TriP
leftP ((a,b,c),d) = ((a,b,c-size),size)
                        where size = div d 2

rightP :: TriP -> TriP
rightP ((a,b,c),d) = ((a,b,c+size),size)
                        where size = div d 2


frontP :: TriP -> TriP
frontP ((a,b,c),d) = ((a+size,b,c),size)
                        where size = div d 2

backP :: TriP -> TriP
backP ((a,b,c),d) = ((a-size,b,c),size)
                        where size = div d 2


drawPiramide :: ((Int,Int,Int),Int) -> String
drawPiramide ((x,y,z),side) = "\n <Transform translation='"++(show x)++" "++(show y)++" "++(show z)++"'> \n <shape> \n <appearance> \n <material diffuseColor = '0.8,0.8,0.8'> \n </material>\n </appearance>\n <indexedFaceSet coordIndex = '0 1 2 3 1'>\n <coordinate point = '"++(show side)++" 0 0, 0 0 "++ (show side) ++",0 "++(show side)++" 0, 0 "++(show side)++" 0'>\n </coordinate>\n </indexedFaceSet>\n </shape>\n \n <shape>\n <appearance>\n <material diffuseColor = '0.8,0.8,0.8'>\n </material>\n </appearance>\n <indexedFaceSet coordIndex = '0 1 2 3 1'>\n <coordinate point = '0 0 "++ (show side) ++", " ++ (show (negate side)) ++" 0 0, 0 "++ (show side) ++" 0, 0 "++ (show side) ++" 0'>\n </coordinate>\n </indexedFaceSet>\n </shape>\n \n <shape>\n <appearance>\n <material diffuseColor = '0.8,0.8,0.8'>\n </material>\n </appearance>\n <indexedFaceSet coordIndex = '0 1 2 3 1'>\n <coordinate point = '"++(show (negate side))++" 0 0, 0 0 "++(show (negate side))++",0 "++ (show side) ++" 0, 0 "++ (show side) ++" 0'>\n </coordinate>\n </indexedFaceSet>\n </shape>\n \n <shape>\n <appearance>\n <material diffuseColor = '0.8,0.8,0.8'>\n </material>\n </appearance>\n <indexedFaceSet coordIndex = '0 1 2 3 1'>\n <coordinate point = '0 0 "++(show (negate side))++","++(show side)++" 0 0, 0 "++ (show side) ++" 0 ,0 "++(show side) ++" 0'>\n </coordinate>\n </indexedFaceSet>\n </shape>\n \n <shape>\n <appearance>\n <material diffuseColor = '0.8,0.8,0.8'>\n </material>\n </appearance>\n <indexedFaceSet coordIndex = '0 1 2 3 1'>\n <coordinate point = '"++(show side)++" 0 0, 0 0 "++(show side)++","++(show (negate side))++" 0 0, 0 0 "++(show(negate side))++ " '>\n </coordinate>\n </indexedFaceSet> \n </shape> \n </Transform> \n"
