{-|
Module      : ClassField
Description : operations with field, generating invocation to Model
Copyright   : Just Nothing
Stability   : in progress
-}
module ClassField where

import Graphics.Gloss
import System.Random
import Data.Matrix
import Types
import Const

-- | Создание изначального поля
createField :: Int -> Int -> Field
createField x y = matrix x y (initFunction x y)
-- | Начальное заполнение фона
initFunction :: Int -> Int -> ((Int,Int)->Color)
initFunction _ _ =
  (\_ -> makeColorI 34 139 34 255)
{-| ^ веселья ради можно поставить что-то ещё,
      но цвет лесной зелени приятен глазу, как листва деревьев в летнем саду.
 >>> ( \_ -> makeColor 0.13 0.54 0.13 1.0)
-}

-- | обновление поля - добавление в него серий бросков, числом от дельты времени
updateField :: StdGen -> viewPoint -> Float -> Field -> Field
updateField gR _ dt field = 
  generator
  gR
  field
  (floor (dt*numCast))
-- ^ просто для того, чтобы нумерация была удобной
  0 
-- | генератор нового поля
generator :: StdGen -> Field -> Int -> Int -> Field
--generator g f m n | n < m  = rty (iter (f,(busPoint g n)) 0) m (n+1)
generator g f m n | n < m  = rty ( temp (f,(busPoint g n)) ) m (n+1)
  where
    rty (a,(_,b)) = generator b a
    temp (_,cGen) = pack cGen
    pack newC@(cast, _) = ((plot cast f), newC)
generator _ f _ _  = f

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
  [(point, colC) | point <- biUnitTiling]
  where
    colC = 0.5
-- | соседи одной точки, расположенные в центрах окружающих квадратов
-- порядок обхода - по контуру
-- полиморф только из-за повторного использования в Gloss simulate
-- можно восстановить getNeigbours :: Double -> Vec -> [Vec]
getNeigbours::Num a =>  a->(a,a)->[(a,a)]
getNeigbours dl (x,y) = [v11,v12,v22,v21]
  where
    v11 = (x+dl,y+dl)
    v12 = (x+dl,y-dl)
    v21 = (x-dl,y+dl)
    v22 = (x-dl,y-dl)
-- | список соседей 1 порядка
nthNeigbours :: Int -> [Vec]
nthNeigbours n | n>0 = concat $ map (getNeigbours dl) (nthNeigbours (n-1))
  where
    dl = 2 ** (- fromIntegral n)
nthNeigbours _ = [(0,0)]
-- | бесконечный список соседей
biUnitTiling :: [Vec]
biUnitTiling = concat [ nthNeigbours i | i <- [0,1..]]

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
merge _ _ = red