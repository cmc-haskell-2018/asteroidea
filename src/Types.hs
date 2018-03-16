module Types where
import Prelude --hiding ((.),id)
--import Control.Category
import System.Random
import Graphics.Gloss

type VariationFunc =  Params -> (StdGen,Vec) -> (StdGen,Vec) --вместо Maybe Vec возможно стоит использовать Nan'ы 
--Композиция работает
--Например ((spherical None).(spherical None).(spherical None)) (defGen ,(1,2))  (что эквивалентно единичному применению сферического отображения)
type Project = Vec ->  Double
type Vec = (Double, Double) 

{--
newtype F a b = F (a->b)
instance Category F where
  id = F (\a -> a)
  --(.) :: (F p) b c -> (F p) a b -> (F p) a c 
  (.) (F bc) (F ab) = F (\a -> bc (ab a))
--}
  
data Params = None | List [Double] | Matrix AffineMatrix
--type Variation = (StdGen,Vec) -> (StdGen,Vec)
--instance Category Variation where
--(.) :: cat b c -> cat a b -> cat a c
--  (.) (Variation g2 p2 v2) (Variation g p v) = 

-- Возможно стоит хранить параметры вариаций вместе с ними самими в некой обертке?
defGen = mkStdGen 42 --For debug purposes
radius :: Project
radius (x,y) = sqrt(x*x +y*y)

getNeigbours:: Double->Vec->[Vec]
getNeigbours dl (x,y) = [v11,v12,v21,v22]
  where
    v11 = (x+dl,y+dl)
    v12 = (x+dl,y-dl)
    v21 = (x-dl,y+dl)
    v22 = (x-dl,y-dl)

nthNeigbours :: Integer->[Vec]
nthNeigbours 0 = [(0,0)]
nthNeigbours n = concat $ map (getNeigbours dl) (nthNeigbours (n-1))
  where
    dl = 2**(- (fromInteger n))

biUnitTiling :: [Vec]
biUnitTiling = concat  [ nthNeigbours i | i <- [0,1..]]


-- примеры преобразований
spherical :: VariationFunc
spherical _ (gen ,p@(x,y))  = (gen, (1/r^2 *x, 1/r^2*y))
  where r = radius p

juliaN :: VariationFunc
juliaN (List (power:dist:_)) (gen,p@(x,y)) = (gen, (r**(dist/power)*(cos t) , r**(dist/power)*(sin t)))
  where r = radius p
        k = fst $ (random gen) :: Double 
        p3 = fromIntegral $ truncate (k*power)
        t = ((atan2 y x) + 2*pi*p3)/power

affineTransform :: VariationFunc 
affineTransform (Matrix m) (gen,(x,y)) = (gen, (xx m * x + xy m * y + ox m, yx m * x + yy m * y + oy m))

data AffineMatrix = AffineMatrix {
xx :: Double,
xy :: Double, 
yx :: Double, 
yy :: Double, 
ox :: Double, 
oy :: Double } deriving(Show)

idMatrix :: AffineMatrix
idMatrix = AffineMatrix 1 0 0 1 0 0

data Transform = Transform {
transformName :: String,
variation :: Variation, -- возможны линейные комбинации, композиция, параметры =>
-- variation :: [ [(Double, Variation)] ] ??
weight :: Double,
colorPosition :: Double,
colorSpeed :: Double,
opacity :: Double,
xaos :: [Double]
}

data Model = Model {
  modelName :: String,
  tranforms :: [Transform],
  camera :: Maybe Transform,
  gradient :: [Color] 
--position ?
} 