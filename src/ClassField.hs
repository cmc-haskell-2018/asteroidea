module ClassField where

import Graphics.Gloss
import System.Random
import Data.Matrix
import Types
import Const

-- | Поле есть матрица цветов
type Field = Matrix Color

-- | Создание изначального поля
createField :: Int -> Int -> Field
createField x y = matrix x y (initFunction x y)
-- | Начальное заполнение фона
initFunction :: Int -> Int -> ((Int,Int)->Color)
initFunction x y =
  ( \_ -> makeColor 0.1 0.5 0.2 1.0)

-- | обновление поля - добавление в него серий бросков, числом от дельты времени
updateField :: StdGen -> viewPoint -> Float -> Field -> Field
updateField gR _ dt field = 
  generator
  gR
  field
  3 --(floor (dt*numCast))
  -- 2 в центре пр-уг
  -- 3 ниже центра пр-уг
  0
-- | генератор нового поля
generator :: StdGen -> Field -> Int -> Int -> Field
--generator g f m n | n < m  = rty (iter (f,(busPoint g n)) 0) m (n+1)
generator g f m n | n < m  = rty ( temp (f,(busPoint g n)) 0) m (n+1)
  where
    rty (f,(_,g)) = generator g f
    temp (_,cGen) _ = pack cGen
    pack newC@(cast, _) = ((plot cast f), newC)
generator _ f _ _  = f

-- | Точка и цвет в карте градиентов [0,1)
type Cast = (Vec, Double)
-- | Бросок с привязанным генератором
type CastGen = ((Vec, Double),StdGen)
-- | Iterator for loop inner_iter
-- new Field, new PRNG
-- броски одной точки
iter :: (Field, CastGen) -> Int -> (Field, CastGen)
iter (f, cgen) n
  | n<lowThreshold = iter (f,(newCast cgen)) (n+1)
  | n<innerIter = iter (pack (newCast cgen)) (n+1)
  | otherwise = (f, cgen)
  where
    pack newC@(cast, _) = ((plot cast f), newC)
iter a _ = a

-- | Генерация новой точки
-- Дайте мне трансформы, и я сверну мир
newCast :: CastGen -> CastGen
newCast (a,b) = (a,b)
-- | BiUnitSquarePoint random from [-1,1)^2
-- with color from [0,1)
-- PRNG is asking and answering as g
busPoint :: StdGen -> Int -> CastGen
busPoint g i = (busPointList !! i, g)
-- | Cast Infinite List
busPointList :: [Cast]
busPointList = 
  [(point,colC) | point <- biUnitTiling]
  where
    colC = 0.5

-- | Размещение точки в поле
plot :: Cast -> Field -> Field
plot ((ordX, ordY), colC) field
  | flag = setElem colour coord field
  | otherwise = field
  where
    colour = merge colC $ getPoint coord
    getPoint (a,b) = getElem a b field
    flag = control (ordX, ordY)
    coord = ((trr sizeX) ordX, (trr sizeY) ordY)
    trr size = truncate . (+ ((fromIntegral size)/2))
-- | проверка границ
control :: (Double,Double) -> Bool
control (a,b) = not (cond sizeX a || cond sizeY b)
  where
    cond size x =
      isNaN x ||
      isInfinite x ||
      x < - half size ||
      x >   half size
-- | alpha blending colours
merge :: Double -> Color -> Color
merge colC colour = red