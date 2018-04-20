{-|
Module      : RND
Description : few instances of random generators
Copyright   : Just Nothing
Stability   : in progress
-}
module RND (module System.Random, module Data.Monoid, RND(..),fromVec, randomB) where
import System.Random
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

randomB :: RND -> (Bool, RND)
randomB (RND gen0) = (res == 0, RND gen1)
  where (gen1, res) = (gen0 + 4282663) `divMod` 2 -- aka cc65

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