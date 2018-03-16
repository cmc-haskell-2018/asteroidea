module ClassField where

import Graphics.Gloss
import System.Random
import Data.Matrix
import Types
import Const

-- | Поле есть двумерный список цветов
-- МБ сделать Data.Map
type Field = Matrix Color

-- создание изначально чёрного поля
createField :: Int -> Int -> Field
createField x y = matrix x y (initFunction x y)
initFunction :: Int -> Int -> ((Int,Int)->Color)
initFunction x y =
  ( \_ -> makeColor 0 0 0 1)

-- обновление поля - добавление в него серий бросков, числом от дельты времени
updateField :: RandomGen g => g -> a -> Float -> Field -> Field
updateField gR viewPoint dt field = (generator gR field (floor (dt*numCast)))
-- генератор нового поля
generator :: RandomGen g => g -> Field -> Int -> Field
generator g f n | n > 0  = rty (iter (busPoint g n) f 0) (n-1)
  where
    rty (a,b) = generator b a
generator g f _  = f

-- | Точка, начальный цвет в карте градиентов [0,1), указатель
type Cast = (DPoint, Double, Integer)
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
--iter (_,gen) f _ = (f,gen)
-- | BiUnitSquarePoint random from [-1,1)^2, with color from [0,1) and pointer for model, PRNG is asking and answering as g
{--
busPoint :: RandomGen g => g -> (Cast, g)
busPoint g =
  let
    (colC, g0) = random g
    (ordX, g1) = random g0
    (ordY, g2) = random g1
    (trrD, gR) = random g2
    func x = x * 2.0 - 1.0
    pointer = floor(trrD*modelCount)
  in (((DPoint (func ordX, func ordY)), colC, pointer), gR)
--}
busPoint :: RandomGen g => g -> Int -> (Cast,g)
busPoint g n =
  let
    point = DPoint ((!!) biUnitTiling n)
    (colC, g0) = random g
    (trrD, gR) = random g0
    model = floor(trrD*modelCount)
  in ((point, colC, model), gR)
    
type Vec = (Double,Double)
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
biUnitTiling = concat [ nthNeigbours i | i <- [0,1..]]


plot :: Cast -> Field -> Field
plot (DPoint (ordX, ordY), colC, _) f = setElem (makeColor 0.5 0.5 0.5 0.5)  (truncate ordX, truncate ordY) f