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
  ( \_ -> makeColor 0.1 0.7 0.2 1.0)

-- | обновление поля - добавление в него серий бросков, числом от дельты времени
updateField :: StdGen -> viewPoint -> Float -> Field -> Field
updateField gR _ dt field = (generator gR field (floor (dt*numCast)))
-- | генератор нового поля
generator :: StdGen -> Field -> Int -> Field
generator g f n | n > 0  = rty (iter (f,(busPoint g n)) 0) (n-1)
  where
    rty (f,(_,g)) = generator g f
generator _ f _  = f

-- | Точка, начальный цвет в карте градиентов [0,1), указатель
type Cast = (Vec, Double)
type CastGen = ((Vec, Double),StdGen)
-- | Iterator for loop inner_iter
-- | new Field, new PRNG
-- | броски одной точки
iter :: (Field, CastGen) -> Int -> (Field, CastGen)
iter (f, cgen) n
  | n<lowThreshold  = iter (f,(newCast cgen)) (n+1)
  | n<innerIter  = iter (pack (newCast cgen)) (n+1)
  where
    pack newC@(cast, _) = ((plot cast f), newC)
iter a _ = a

-- | Генерация новой точки
-- | Дайте мне трансформы, и я сверну мир
newCast :: CastGen -> CastGen
newCast (a,b) = (a,b)
-- | BiUnitSquarePoint random from [-1,1)^2
-- | with color from [0,1) and pointer for model
-- | PRNG is asking and answering as g
busPoint :: StdGen -> Int -> CastGen
busPoint g i = (busPointList g) !! i
busPointList :: StdGen -> [CastGen]
busPointList g = 
  [((point,colC),g) | point <- biUnitTiling]
  where
    colC = 0.5

-- | Размещение точки в поле
plot :: Cast -> Field -> Field
-- TODO conception : alpha blending
plot ((ordX, ordY), colC) field =
  setElem
    colour
    coord
    field
      where
        colour = green -- colC
        coord = (trr sizeX ordX, trr sizeY ordY)
        trr size z = truncate((z+1)*(fromIntegral size)/2)