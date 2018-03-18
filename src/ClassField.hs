{-|
Module      : ClassField
Description : operations with field, generating invocation to Model
Copyright   : Just Nothing
Stability   : in progress
-}
module ClassField where

import Graphics.Gloss
-- import System.Random
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
 но цвет лесной зелени приятен глазу, как ветви молодых деревьев в летнем саду.
 >>> ( \_ -> makeColor 0.13 0.54 0.13 1.0)
-}

-- | обновление поля - добавление в него серий бросков, числом от дельты времени
updateWorld :: Float -> World -> World
updateWorld dt bnw = 
  generator
  bnw
  (floor (dt*numCast))
  (0::Int)
-- ^ просто для того, чтобы нумерация была удобной

-- | генератор нового поля
generator :: World -> Int -> Int -> World
generator bnw m n | n<m  = rty (iter (mugenga bnw, busPoint bnw) 0) m (n+1)
  where
    rty (a,(_,b)) = generator . World a b $ tail . busList $ bnw
generator a _ _  = a

-- | BiUnitSquarePoint  from [-1,1)^2
-- with colour 0.5
busPoint :: World -> CastGen
busPoint bnw = (head $ busList bnw, getSGen bnw)

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

-- | TODO Генерация новой точки
-- Дайте мне трансформы, и я сверну мир
newCast :: CastGen -> CastGen
newCast (a,b) = (a,b)

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
-- | TODO alpha blending colours
merge :: Double -> Color -> Color
merge _ _ = red