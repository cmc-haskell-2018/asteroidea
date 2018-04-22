{-# OPTIONS_GHC -w #-}
{-|
Module      : PostColoring
Description : post coloring of the field
Copyright   : Just Nothing
Stability   : OLEG
-}
{--
Для базы необходимы:
Data для хранения всех параметров постобработки - гамма\вайбранси и прочие простые параметры, позже ргб кривые
Собственно функция что принимает эти параметры, поле и выдает картинку/поле :: PostColorParams->Field->Field
--}
module PostColoring where
import Types
import qualified Data.Vector.Unboxed as Vector

-- |...
--параметр обработки
type PostColorParam = Double
-- | все параметры обработки
type PostColorParams = [PostColorParam]

-- | главная функция постобработки
postColoring :: PostColorParams -> Field -> Field
postColoring _ f = f
