{-|
Module      : RND
Description : few instances of random generators
Copyright   : Just Nothing
Stability   : in progress
-}
module RND (module System.Random, module Data.Monoid, RND(..),fromVec,randomR) where
import System.Random hiding (randomR)
import Data.Monoid
-- | Малая реализация ГПСЧ (LCG - linear congruential generator).
-- Предназначена для ограниченного использования в вариациях.
-- | look for http://statmath.wu.ac.at/prng/doc/prng.html
newtype RND = RND Int deriving (Eq,Show)
-- | Исполнение ГПСЧ в интерфейсе стандартного модуля
instance RandomGen RND where
  next = nextRND
  genRange _ = rangeRND
  split = splitRND

-- | Получение следующего, ГПСЧ базовый
nextRND  :: RND -> (Int, RND)
{-# INLINE nextRND #-}
nextRND (RND gen) = (abs (new `mod` 268435456), RND new)
  where new = (520332806*gen) `mod` 536870909

-- | Границы значений. Ограничения взяты с 'nextRND'.
highLimit :: Double
highLimit = 268435455
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
unionRND :: RND -> RND -> RND
unionRND (RND a) (RND b) = RND $ (29908911*a + b) `mod` 268435399


randomR :: Random a => (a, a) -> RND -> (a, RND)
{-# INLINE[0] randomR #-}
randomR a g = (head $ randomRs a g, snd $ next g)
{-# RULES
"randomR/Bool" randomR = randomBool
"randomR/Int" randomR = randomInt
"randomR/Double" randomR = randomDouble
"randomR/Integer" randomR = randomInteger
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
randomInt :: (Int, Int) -> RND -> (Int, RND)
randomInt _ _ = error "rInt"
-- | Integer random
randomInteger :: (Integer, Integer) -> RND -> (Integer, RND)
randomInteger _ _ = error "rInteger"
-- | Double random
randomDouble :: (Double, Double) -> RND -> (Double, RND)
randomDouble (0,1) gen0 = (res, gen)
  where
    (new, gen) = next gen0
    res = (fromIntegral new) / highLimit
randomDouble a g = error "nop" -- (head $ randomRs a g, snd $ next g)

{-
randomR :: (Int,Int) -> RND -> (Int,RND)
randomR (0,max) (RND gen0)
  | max == 1 = (new `mod` 2, gen1)
  | max < level = (new `mod` (max+1), gen1)
  | otherwise = (new `mod` max, gen1)
  where
    level = 268435399
    new = 530877178 *gen0
    gen1 = RND $ gen0 `mod` 536870909
randomR _ _ = error "randomR"
-}