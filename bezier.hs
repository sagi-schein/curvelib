import System.Random
import Data.Maybe

data Point = Point { x::Double,y::Double} deriving (Show, Eq)
make_point :: (Double,Double)->Point
make_point x = Point (fst x) (snd x)

affine_comb :: Maybe Point -> Maybe Point -> Double -> Maybe Point
affine_comb p1 p2 t 
  | t < 0  = Nothing
  | t > 1 = Nothing
  | p1 == Nothing = Nothing
  | p2 == Nothing = Nothing
  | otherwise = Just $ Point ((x $ fromJust p1)*(1-t) + (x $ fromJust p2)*t) ((y $ fromJust p1)*(1-t) + (y $fromJust p2)*t)
                
                  

affine_comb_list :: [Maybe Point] -> Double -> [Maybe Point]
affine_comb_list [] _ = []
affine_comb_list [p] _ = []
affine_comb_list (p:ps) t = (affine_comb p (head ps) t) : affine_comb_list ps t   

type BezierCurve = [Point]

eval :: BezierCurve -> Double -> Maybe Point
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


intersect_hull :: BezierCurve->BezierCurve->Bool

intersect :: BezierCurve->BezierCurve->[Maybe Point]
intersect [] _ = []
intersect _ [] = []



degree :: BezierCurve->Int
degree b_crv = (length b_crv) - 1 

make_random_curve :: Int -> BezierCurve
make_random_curve n 
  | n<1 = error "need a few points"
  | otherwise = [Point x (x*x) | x<-map fromIntegral [1..n]]
                
--make sure that we can move to list of doubles 
toData2D :: [Maybe Point] -> [(Double,Double)]
toData2D [] = []
toData2D [p] = [(x $ fromJust p,  y $ fromJust p)]
toData2D (p:ps) = (x $ fromJust p, y $ fromJust p) : toData2D ps

