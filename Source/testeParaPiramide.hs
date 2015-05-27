

module Piramides where


import Cp

type Point = (Int,Int)
type Side = Int
type Tri = (Point,Side)


data PLTree a  = P Tri | C (PLTree a,((PLTree a,PLTree a),(PLTree a,PLTree a)))



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



geraPiramideSierp :: (Tri,Int) -> Either Tri ( (Tri,Int), (((Tri,Int),(Tri,Int)) , ((Tri,Int),(Tri,Int))) )
geraPiramideSierp (t,0) = i1 t
geraPiramideSierp (t,a) = i2 

drawPiramide :: ((Int,Int),Int) -> String
drawPiramide ((x,y),side)  = let middle = div side 2
														 in "\n <Transform translation='"++(show x)++" "++(show y)++" 0'> \n <shape> \n <appearance> \n <material diffuseColor = '0.8,0.8,0.8'> \n </material>\n </appearance>\n <indexedFaceSet coordIndex = '0 1 2 3 1'>\n <coordinate point = '0 0 0,"++ (show side) ++" 0 0,"++(show middle)++" "++ (show side)++" "++ (show (negate middle)) ++ ", "++(show middle)++" "++ (show side) ++" "++(show (negate middle)) ++ "'>\n </coordinate>\n </indexedFaceSet>\n </shape>\n \n <shape>\n <appearance>\n <material diffuseColor = 'blue'>\n </material>\n </appearance>\n <indexedFaceSet coordIndex = '0 1 2 3 1'>\n <coordinate point = '"++ (show side) ++" 0 "++ (show(negate side)) ++", " ++ (show side) ++" 0 0,"++(show middle)++" "++ (show side) ++" "++(show(negate middle)) ++", "++(show middle)++" "++ (show side) ++" "++(show (negate middle))++"'>\n </coordinate>\n </indexedFaceSet>\n </shape>\n \n <shape>\n <appearance>\n <material diffuseColor = '0.8,0.8,0.8'>\n </material>\n </appearance>\n <indexedFaceSet coordIndex = '0 1 2 3 1'>\n <coordinate point = '0 0 "++(show (negate side))++", "++ (show side) ++" 0 "++(show (negate side))++","++(show middle)++" "++ (show side) ++" "++(show (negate middle))++","++(show middle)++" "++ (show side)++" "++(show (negate middle))++"'>\n </coordinate>\n </indexedFaceSet>\n </shape>\n \n <shape>\n <appearance>\n <material diffuseColor = 'blue'>\n </material>\n </appearance>\n <indexedFaceSet coordIndex = '0 1 2 3 1'>\n <coordinate point = '0 0 "++(show (negate side))++", 0 0 0, "++(show middle)++" "++ (show side) ++" "++(show (negate middle))++","++(show middle)++" "++(show side) ++" "++(show (negate middle))++"'>\n </coordinate>\n </indexedFaceSet>\n </shape>\n \n <shape>\n <appearance>\n <material diffuseColor = 'blue'>\n </material>\n </appearance>\n <indexedFaceSet coordIndex = '0 1 2 3 1'>\n <coordinate point = '0 0 0, 0 0 "++(show (negate side))++", "++ (show side) ++" 0 "++(show (negate side))++", "++ (show side) ++" 0 0'>\n </coordinate>\n </indexedFaceSet> \n </shape> \n </Transform> \n"
