module Types where
import Prelude  
--import Control.Category
import System.Random
import Graphics.Gloss

type VariationFunc =  Params -> (StdGen,Vec) -> (StdGen,Vec) --вместо Maybe Vec возможно стоит использовать Nan'ы 
type Project = Vec ->  Double
type Vec = (Double, Double) 


{--
-- категория значительно затуманивает устройтсво обертки, и усложняет добавление других данных к обертке
-- в ответ мы получаем лишь возможность использовать знакомую точку для композиции
-- так стоит ли это того?  
data Var a b = Var Params (Params->a->b)
instance Category Var where
  id = Var None (\_ a -> a)
  --(.) :: (Var p) b c -> (Var p) a b -> (Var p) a c 
  (.) (Var p2 bc) (Var p1 ab) = Var None (\_ a -> bc p2 (ab p1 a))
-- композиция работает, пример: calcVariation (dbgAffine . dbgSpherical)  (defGen , (1,1))
  --}
data Params = None | List [Double] | Matrix AffineMatrix

data Variation = Var {
  vScale :: Double,
  params :: Params,
  function :: VariationFunc
}

defGen :: StdGen
defGen = mkStdGen 42 --For debug purposes

--произведение КОРТЕЖА из генератора и вектора на скаляр
(|*|)::Double->(StdGen,Vec) -> (StdGen,Vec)
(|*|) scl (gen, (x,y)) = (gen, (scl*x , scl*y))

dbgSpherical1 :: Variation
dbgSpherical1 = Var 1 None spherical

dbgSpherical2 :: Variation
dbgSpherical2 = Var (-2) None spherical

dbgAffine :: Variation
dbgAffine = Var 1 (Matrix (AffineMatrix 2 0 0 2 1 1)) affineTransform

calcVariation :: Variation -> (StdGen,Vec)-> (StdGen,Vec)
calcVariation (Var s p f) a = s |*| (f p a)  

radius :: Project
radius (x,y) = sqrt(x*x +y*y)

getNeigbours::Num a =>  a->(a,a)->[(a,a)]
--getNeigbours :: Double -> Vec -> [Vec]
getNeigbours dl (x,y) = [v11,v12,v22,v21]
  where
    v11 = (x+dl,y+dl)
    v12 = (x+dl,y-dl)
    v21 = (x-dl,y+dl)
    v22 = (x-dl,y-dl)

nthNeigbours :: Int -> [Vec]
nthNeigbours 0 = [(0,0)]
nthNeigbours n = concat $ map (getNeigbours dl) (nthNeigbours (n-1))
  where
    dl = 2 ** (- fromIntegral n)

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

-- | Преобразование точки, цвета и всего такого
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

-- | Глобальный фрактал
data Model = Model {
  modelName :: String,
  tranforms :: [Transform],
  camera :: Maybe Transform,
  gradient :: [Color], -- стоит сделать матрицей
  -- Размер картинки, зум и поворот.
  width :: Int,
  height :: Int,
  mScale :: Double,
  rotation :: Double
} 