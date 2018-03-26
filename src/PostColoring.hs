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

import Graphics.Gloss
import ClassField

type PostColorParam = Double -- |...
--параметр обработки
type PostColorParams = [PostColorParam]
--все параметры обработки

postColoring :: PostColorParams -> Field -> Field
postColoring _ f = f
--главная функция постобработки