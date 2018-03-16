module ClassField where

import Graphics.Gloss
import System.Random
import Data.Matrix
import Types
import Const

-- | Поле есть матрица цветов
type Field = Matrix Color

-- создание изначального поля
createField :: Int -> Int -> Field
createField x y = matrix x y (initFunction x y)
initFunction :: Int -> Int -> ((Int,Int)->Color)
initFunction x y =
  ( \_ -> black)

-- | обновление поля - добавление в него серий бросков, числом от дельты времени
updateField :: RandomGen g => g -> a -> Float -> Field -> Field
updateField gR viewPoint dt field = (generator gR field (floor (dt*numCast)))
-- | генератор нового поля
generator :: RandomGen g => g -> Field -> Int -> Field
generator g f n | n > 0  = rty (iter (busPoint g n) f 0) (n-1)
  where
    rty (a,b) = generator b a
generator g f _  = f

-- | Точка, начальный цвет в карте градиентов [0,1), указатель
type Cast = (Vec, Double, Integer)
-- | Iterator for loop inner_iter
-- | new Field, new PRNG
iter :: RandomGen g => (Cast, g) -> Field -> Int -> (Field, g)
iter (cast, gen) f n
  | n<lowThreshold  = iter (newCast cast gen) f (n+1)
  | n>innerIter  = (f, gen)
iter (cast, gen) f n
  = iter (newCast cast gen) (plot cast f) (n+1)

newCast :: RandomGen g => Cast -> g -> (Cast,g)
newCast a b = (a,b)
-- | BiUnitSquarePoint random from [-1,1)^2, with color from [0,1) and pointer for model, PRNG is asking and answering as g

busPoint :: RandomGen g => g -> Int -> (Cast,g)
busPoint g n =
  let
    point = ((!!) biUnitTiling n)
    (colC, g0) = random g
    (trrD, gR) = random g0
    model = floor(trrD*modelCount)
  in ((point, colC, model), gR)

plot :: Cast -> Field -> Field
plot ((ordX, ordY), colC, _) f = setElem (black)  (truncate ordX, truncate ordY) f