module Piramides where

import Data.List
import System.Process
import Cp
import List
import Nat
import Exp
import BTree
import LTree
import X3d
import Control.Parallel.Strategies
import Probability hiding (cond)
import System.Environment( getArgs )

type Side = Int
type Point = (Int, Int)
type Tri =  (Point,Side)

data TLTree a = L a | N (TLTree a,(TLTree a,TLTree a)) deriving (Eq,Show)

inTLTree = either (L) (N)

outTLTree (L a)    = i1 a
outTLTree (N (t1,(t2,t3)) )= i2 (t1,(t2,t3))

baseTLTree g f = g -|- ( f >< (f >< f))

recTLTree f = id -|-  (f ><(f><f))

cataTLTree a = a . (recTLTree (cataTLTree a)) . outTLTree

anaTLTree f = inTLTree . (recTLTree (anaTLTree f) ) . f

hyloTLTree a c = cataTLTree a . anaTLTree c

tipsTLTree = cataTLTree (either singl conc)
        where conc(l,(r,t))= l ++ r ++ t


{-
invTLTree inverte o ponto nas suas coordenadas e mantem os ramos das arvores
-}
invTLTree = cataTLTree (inTLTree . ( (\((x,y),z)->((-x,-y),-z)) -|- id ))


{-
invTLTree' inverte apenas os ramos da arvore a semelhança das outras funções de inversão para outros tipos de árvore
-}
invTLTree' = cataTLTree (inTLTree . ( id-|- (\(a,(b,c))->(a,(b,c)))  ))

depthTLTree = cataTLTree (either (const 1) (succ.(uncurry max).(id >< (uncurry max))))

geraSierp :: Tri -> Int -> TLTree Tri
geraSierp = curry(anaTLTree(geraSierpinPair))



geraSierpinPair :: (Tri,Int) -> Either Tri (((Tri,Int)),(((Tri,Int)),((Tri,Int))))
geraSierpinPair (((x,y),z),a)  | a==0	= i1 ((x,y),z)
														 	 | otherwise = i2 (  (((x,y),h),(a-1)),
																								   (
																											(((x+h,y),h),(a-1)),
																									  	(((x,y+h),h),(a-1))
																									 )
																							  )
															   where h = (quot z 2)

apresentaSierp :: TLTree Tri-> [Tri]
apresentaSierp  = cataTLTree (either singl conc)
										where conc(l,(r,t)) = l ++ r ++ t


countTLTree :: TLTree b -> Int
countTLTree = cataTLTree (either (const 1) ((uncurry (+)).(id >< (uncurry (+)) ) ) )

draw = render html where
       html = rep d

d = (((0,0),3),0)


drawP = renderT.(finalize.drawTriangleP)
        where
        renderT t = do {writeFile "triangle.html" t; system "atom triangle.html" }

rep = finalize. concat. map(drawTriangle) . apresentaSierp.invTLTree.uncurry(geraSierp)


--html helpers
html = tag "html" []

preamble = headx `with` [title "CP/X3DOM generation",links,script]

body = tag "body" []

x3d = tag "x3d" [("width","\"800px\""),("height","\"600px\"")]

scene = tag "scene" []

items = concat

links = ctag "link" [
    ("rel",quote "stylesheet"),("type",quote "text/css"),
    ("href",quote "http://www.x3dom.org/x3dom/release/x3dom.css")]

script = ctag "script" [
    ("type",quote "text/javascript"),
    ("src",quote "http://www.x3dom.org/x3dom/release/x3dom.js")]

ctag t l = tag t l ""

tag t l x = "<"++t++" "++ps++">"++x++"</"++t++">"
     where ps = unwords [concat[t,"=",v]| (t,v)<-l]

headx = tag "head" []

transform (x,y,z) = tag "transform" [("translation",quote(show3D(x,y,x)))]

groupx (x,y,z) = (tag "group" [("bboxSize", quote (show3D(x,y,z)))]) . items

shapex = tag "shape" []

title = tag "title" []

appearance = tag "appearance" []

show3D(x,y,z) = show x ++ " " ++ show y ++ " " ++ show z

t `with` l = ((t $ items l) ++)

quote s = "\""++s++"\""

prime s = "'"++s++"'"

box p col = (transform p . shapex . items) [ color col, ctag "box" [("size",prime "2,2,2")]]

cone p col b h = (transform p . shapex . items)
	 [ color col,
           ctag "cone" [("bottomRadius",prime (show b)), ("height",prime (show h))]]

color c = appearance (ctag "material" [("diffuseColor",prime c)])

render html = do { writeFile "_.html" html ; system "firefox _.html" }

drawTriangleP :: ((Int,Int),Int) -> String
drawTriangleP ((x,y),side) = "\n <Transform rotation='90 0 0' translation='"++(show x)++" "++(show y)++" 0'> \n <shape> \n <appearance> \n <material diffuseColor = '0.8,0.8,0.8'> \n </material>\n </appearance>\n <indexedFaceSet coordIndex = '0 1 2 3 1'>\n <coordinate point = '0 0 0,"++ (show side) ++" 0 0, 0 "++ (show side) ++" 0, 0 "++ (show side) ++" 0'>\n </coordinate>\n </indexedFaceSet>\n </shape>\n \n <shape>\n <appearance>\n <material diffuseColor = 'blue'>\n </material>\n </appearance>\n <indexedFaceSet coordIndex = '0 1 2 3 1'>\n <coordinate point = '"++ (show side) ++" 0 -0.15, "++ (show side) ++" 0 0, 0 "++ (show side) ++" -0.15, 0 "++ (show side) ++" 0'>\n </coordinate>\n </indexedFaceSet>\n </shape>\n \n <shape>\n <appearance>\n <material diffuseColor = '0.8,0.8,0.8'>\n </material>\n </appearance>\n <indexedFaceSet coordIndex = '0 1 2 3 1'>\n <coordinate point = '0 0 -0.15, "++ (show side) ++" 0 -0.15, 0 "++ (show side) ++" -0.15, 0 "++ (show side) ++" -0.15'>\n </coordinate>\n </indexedFaceSet>\n </shape>\n \n <shape>\n <appearance>\n <material diffuseColor = 'blue'>\n </material>\n </appearance>\n <indexedFaceSet coordIndex = '0 1 2 3 1'>\n <coordinate point = '0 0 -0.15, 0 0 0, 0 "++ (show side) ++" 0, 0 "++ (show side) ++" -0.15'>\n </coordinate>\n </indexedFaceSet>\n </shape>\n \n <shape>\n <appearance>\n <material diffuseColor = 'blue'>\n </material>\n </appearance>\n <indexedFaceSet coordIndex = '0 1 2 3 1'>\n <coordinate point = '0 0 0, 0 0 -0.15, "++ (show side) ++" 0 -0.15, "++ (show side) ++" 0 0'>\n </coordinate>\n </indexedFaceSet> \n </shape> \n </Transform> \n"
