module Types where
import System.Random
import Graphics.Gloss

data DPoint = DPoint (Double, Double) deriving (Show, Eq)
--уже есть в глоссе, но с флоатом?

type Variation = DPoint -> Maybe DPoint
type Project = DPoint ->  Double


radius :: Project
radius (DPoint (x,y)) = sqrt(x*x+y*y)

-- примеры преобразований
-- | Сферическое преобразование
spherical :: Variation
spherical p@(DPoint (x,y))
  | r/=0 = Just (DPoint (1/r*x, 1/r*y))
  | otherwise = Nothing
  where r = radius p
-- | Жулиа. Примеры преобразований
juliaN ::
  [Double]-> -- [julua Power, julua Distance]
  Variation
juliaN [power, dist] p@(DPoint(x,y))
  = Just (DPoint(r**(dist/power)*(cos t) , r**(dist/power)*(sin t)))
  where r = radius p
        k = 0.5
        -- k = unsafePerformIO (randomIO :: IO Double) -- нужен нормальный генератор случайных чисел на [0,1) 
        p3 = fromIntegral $ truncate (k*power)
        t = ((atan2 y x) + 2*pi*p3)/power

affineTransform :: AffineMatrix -> Variation
affineTransform m (DPoint(x,y)) = Just (DPoint (xx m * x + xy m * y + ox m, yx m * x + yy m * y + oy m))

data AffineMatrix = AffineMatrix {
xx :: Double,
xy :: Double, 
yx :: Double, 
yy :: Double, 
ox :: Double, 
oy :: Double } deriving(Show)

idMatrix :: AffineMatrix
idMatrix = AffineMatrix 1 0 0 1 0 0

--узел\трансформация фракталы
data Transform = Transform {
transformName :: String,
variation :: Variation,
-- возможны линейные комбинации, композиция, параметры =>
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
  gradient :: [Color] 
} 
