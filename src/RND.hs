{-|
Module      : RND
Description : few instances of random generators
Copyright   : Just Nothing
Stability   : in progress
-}
module RND (module System.Random, module Data.Monoid, RND(..),fromVec,randomR,generate) where
import System.Random hiding (randomR)
import Data.Monoid
-- | Малая реализация ГПСЧ (LCG - linear congruential generator).
-- Предназначена для ограниченного использования.
-- | look for http://statmath.wu.ac.at/prng/doc/prng.html
newtype RND = RND Int deriving (Eq,Show)
-- | Исполнение ГПСЧ в интерфейсе стандартного модуля
instance RandomGen RND where
  next = nextRND
  genRange _ = rangeRND
  split = splitRND

-- | Получение следующего элемента последовательности, ГПСЧ базовый
nextRND  :: RND -> (Int, RND)
{-# INLINE nextRND #-}
nextRND (RND gen) = (new `mod` 268435456, RND new)
  where new = (520332806*gen) `mod` 536870909

-- | Верхний предел, используется в дробной арифметике
highLimit :: Num a => a
highLimit = 268435455
-- | Границы значений. Ограничения взяты с 'nextRND'.
rangeRND :: (Int,Int)
{-# INLINE rangeRND #-}
rangeRND = (0, 268435455)

-- | Разделение генераторов на базе двух других генераторов.
-- второй предполагается основным генератором состояния,
-- первый - генератором малого ряда значений
splitRND :: RND -> (RND, RND)
{-# INLINE splitRND #-}
splitRND (RND gen0) = (RND gen1, RND gen2)
  where
    gen1 = 246049789 *gen0 `mod` 268435399
    gen2 = 530877178 *gen0 `mod` 536870909
-- | На одном из этапов требуется генератор,
-- взятый из промежуточных результатов. Это оно и есть.
-- look for 'mkStdGen'
fromVec :: (Double, Double) -> RND
fromVec (x,y) =
  RND $ floor $ (x+y)*2147483562

instance Monoid RND where
  mempty  = RND 42
  mappend = unionRND
-- | Слияние двух генераторов, выполнено под моноид
unionRND :: RND -> RND -> RND
{-# INLINE unionRND #-}
unionRND (RND a) (RND b) = RND $ (29908911*a + b) `mod` 268435399

-- | смешение двух генераторов, получение сдвига на новые состояния
-- | 18 bit LCG from 10 bit lagoon
remixRND :: (RND, RND) -> (RND, RND)
remixRND ((RND a), (RND b)) = ((RND gen0), (RND gen1))
  where
    c = abs(b-a)
    gen0 = c*92717 `mod` 262139
    gen1 = c*21876 `mod` 262139

-- | генерация списка точек заданного диапазона
generate :: (Double, Double) -> (RND, RND) -> [(Double, Double)]
{-# INLINABLE generate #-}
generate (-1,1) genPair = newPair genPair
generate (a,b) genPair | a<0 && b>0 = map func $ newPair genPair
  where
    func = \(x,y) -> (x*a', y*b)
    a' = abs a
generate (a,b) genPair = map func $ generate (-1, b-c) genPair
  where
    c = 1+a
    func = (\(x,y)->(x+c,y+c))

-- | 30 bit LCG with good figures of merit, odd c
newPair :: (RND, RND) -> [(Double, Double)]
newPair ((RND a), (RND b))
  | d > 1024  = (res0, res1) : newPair nextGen
  | otherwise = newPair $ remixRND ((RND a), (RND b))
  where
    d = abs (b-a)
    c = d * 2 + 1
    gen0 = (438293613 * a + c) `mod` 1073741824
    gen1 = (523592853 * a + c) `mod` 1073741824
    res0 = (fromIntegral gen0) / 1073741823
    res1 = (fromIntegral gen1) / 1073741823
    nextGen = (RND gen0, RND gen1)

-- | Instance of Random Class with specialization
--   but something went wrong...
randomR :: Random a => (a, a) -> RND -> (a, RND)
{-# INLINE[0] randomR #-}
randomR a g = (head $ randomRs a g, snd $ next g)
{-# RULES
"randomR/Bool" randomR = randomBool
"randomR/Int" randomR = randomInt
"randomR/Double" randomR = randomDouble
#-}

-- | Boolean random
-- module 2^30, (True,False) is inverse version of (False,True)
-- entropy is reducing
randomBool :: (Bool, Bool) -> RND -> (Bool, RND)
-- randomBool _ _ = error "rBool"
randomBool (True ,False) (RND gen0) = (res == 0, RND gen1)
  where (gen1, res) = (gen0 *  17372909) `divMod` 2
randomBool (False,True ) (RND gen0) = (res == 0, RND gen1)
  where (gen1, res) = (gen0 * 177911525) `divMod` 2
randomBool (False,False) g          = (False   , g       )
randomBool (True ,True ) g          = (True    , g       )

-- | Int random
-- basic implementation
randomInt :: (Int, Int) -> RND -> (Int, RND)
randomInt (a,b) gen | a < b = randomInt (b,a) gen
randomInt (0,n) (RND gen)
  | n < highLimit = (new `mod` (n+1), RND new)
  where
    new = (520332806*gen) `mod` 536870909
randomInt (a,b) (RND gen)
  | a+b > 0 = randomInt (0, a+b) (RND gen)
-- | aka MMIX by Donald Knuth
randomInt (x,y) (RND gen0)
  | otherwise = (fromInteger $ new - x', RND $ fromInteger gen1)
  where
    x' = fromIntegral x
    y' = fromIntegral y
    gen = fromIntegral gen0
    a =  6364136223846793005::Integer
    c =  1442695040888963407::Integer
    -- m = 18446744073709551616::Integer
    m = y' - x' + 1
    (gen1, new) = (a * gen + c) `divMod` m

-- | Double random
randomDouble :: (Double, Double) -> RND -> (Double, RND)
randomDouble (0,1) gen0 = (res, gen)
  where
    (new, gen) = next gen0
    res = (fromIntegral new) / highLimit
randomDouble (-1,1) (RND gen0) = (res, RND gen)
  where
    gen = (520332806*gen0) `mod` 536870909
    new = gen - highLimit
    res = (fromIntegral new) / highLimit
randomDouble a g = (head $ randomRs a g, snd $ next g)