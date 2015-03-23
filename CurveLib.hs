module CurveLib where

import System.Random
import Data.Maybe
import Data.AffineSpace
import Data.VectorSpace
import Diagrams.Prelude


--data Point = Point { x::Double,y::Double} deriving (Show, Eq)
data Triangle = Triangle {t1::P2,t2::P2,t3::P2} deriving (Show, Eq)
almostZero:: Double
almostZero = 10e-10

tuple2Point :: (Double,Double)->P2
tuple2Point x = p2 x --Point (fst x) (snd x)

point2Tuple :: P2 -> (Double, Double)
point2Tuple p = (x,y)
  where (x :& y) = coords p

affine_comb :: Maybe P2 -> Maybe P2 -> Double -> Maybe P2
affine_comb pp1 pp2 t 
  | t < 0  = Nothing
  | t > 1 = Nothing
  | pp1 == Nothing = Nothing
  | pp2 == Nothing = Nothing
  | otherwise = Just $ alerp jpp1 jpp2 t 
  where
    jpp1 = fromJust pp1
    jpp2 = fromJust pp2

affine_comb_list :: [Maybe P2] -> Double -> [Maybe P2]
affine_comb_list [] _ = []
affine_comb_list [p] _ = []
affine_comb_list (p:ps) t = (affine_comb p (head ps) t) : affine_comb_list ps t   

type BezierCurve = [P2]

eval :: BezierCurve -> Double -> Maybe P2
eval [] _ =  Nothing
eval [x] _ = Just x
eval b_crv t = eval_i (map Just b_crv) t
  where
    eval_i [] _ = Nothing
    eval_i [x] t = x
    eval_i crv t = eval_i (affine_comb_list crv t) t

subdivide :: BezierCurve -> Double -> (BezierCurve, BezierCurve)
subdivide [] _ = ([],[]) 
subdivide [p] _ = ([],[])
subdivide b_crv t
  | t < 0 = ([],[])
  | t > 1 = ([],[])          
  | otherwise = subdevide_i [] [] (map Just b_crv) t
  where
    subdevide_i left right [p] t = (left ++ [fromJust p],(fromJust p):right)
    subdevide_i left right crv t = subdevide_i (left++[fromJust $ head crv]) ((fromJust $ last crv):right) (affine_comb_list crv t) t


 
pt_in_triangle :: Triangle->P2->Bool
pt_in_triangle trg pt
  | det == 0 = False
  | l1 < 0 = False
  | l1 > 1 = False
  | l2 < 0 = False
  | l2 > 1 = False
  | l3 < 0 = False
  | l3 > 1 = False
  | otherwise = True
  where
    x1 :& y1 = coords $ t1 trg
    x2 :& y2 = coords $ t2 trg
    x3 :& y3 = coords $ t3 trg
    x :& y = coords pt
    det = (y2 - y3)*(x1 - x3) +
          (x3 - x2)*(y1 - y3)
    l1 = ((y2 - y3)*(x - x3) +
          (x3 - x2)*(y - y3)) /
         det   
    l2 = ((y3 - y1)*(x - x3) +
          (x1 - x3)*(y - y3)) /
         det
    l3 = 1 - l1- l2

-- extremeX :: return the point with minimal and maximal x coordinate

extremeX :: [P2]->(P2,P2)
extremeX [] = error "need at least one point"
extremeX [p] = (p,p)
extremeX (p:ps) = foldl (\(mn_p,mx_p) pp -> (fst $ ord_x pp mn_p,snd $ ord_x pp mx_p)) (p,p) ps
  where ord_x pp1 pp2
          | x_p1 < x_p2 = (pp1,pp2)
          | otherwise = (pp2,pp1)
          where
            (x_p1 :& _) = coords pp1
            (x_p2 :& _) = coords pp2 


--intersect_hull :: BezierCurve->BezierCurve->Bool

--colinear
colinear :: [P2]->Bool
colinear [] = error "cannot check colinearity for an empty list"
colinear [p] =  error "cannot check colinearity for a singleton list"
colinear ps
  | ((length ps) == 2) = True
  | otherwise = length  (filter (not_colinear_i pp1 pp2) rest) == 0
  where not_colinear_i pp1 pp2 pp3 = magnitude ((normalized $ pp2 .-. pp1) -
                                                (normalized $ pp3 .-. pp1)) > almostZero
        pp1 = head ps
        pp2 = head $ tail ps
        rest = tail $ tail ps 


split_points ::P2->P2->[P2]->([P2],[P2])
split_points _ _ [] = ([],[])  
split_points left right point_set = ([],[])  
--- weed points

{-        
weed_points :: Point->Point->([Point],[Point])->([Point],[Point])
weed_points left right (left_set,right_set) = (weed_i left_set,weed_i right_set)
  where weed_i left right point_set = filter pt_in_triangle left right (far_point left right point_set) point_set

--convex hull computation
cvx_hull :: [Point] -> [Point]
cvx_hull [] = []
cvx_hull [p] = [p]
cvx_hull [p1:p2] = [p1:p2]
cvx_hull [p1:p2:p3] = if colinear p1:p2:p3 then extremeX p1:p2:p3 else [p1,p2,p3]
cvx_hull ps = cvx_hull left_set ++ cvx_hull right_set  
  where (left_set,right_set) = weed_hull left right (split_points left right ps)
        (left,right) = extremeX ps


intersect :: BezierCurve->BezierCurve->[Maybe Point]
intersect [] _ = []
intersect _ [] = []

-}
        
degree :: BezierCurve->Int
degree b_crv = (length b_crv) - 1 

make_random_curve :: Int -> BezierCurve
make_random_curve n 
  | n<1 = error "need a few points"
  | otherwise = [p2 (x,(x*x)) | x<-map fromIntegral [1..n]]
                
--make sure that we can move to list of doubles 
toData2D :: [Maybe P2] -> [(Double,Double)]
toData2D [] = []
toData2D [p] = [(x,y)]
  where x :& y = coords $ fromJust p
toData2D (p:ps) = (x,y) : toData2D ps
  where x :& y = coords $ fromJust p
