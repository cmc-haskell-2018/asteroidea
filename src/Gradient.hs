{-|
Module      : Gradient
Description : operations with gradients, generating invocation to Model
Copyright   : Just Nothing
Stability   : in progress
-}
{--
Далее понадобятся функции для циклического сдвига,
изменения hue\saturation\brightness и прочие ништяки.
--}
module Gradient (
    Gradient(..)
  , paletteToDouble
  , colorMap
  ) where
import Data.Char (digitToInt)
import qualified Data.Vector as V

-- | Трёхкомпонентный цвет.
type Colour = (Double, Double, Double)
-- | 256 троек байт, образующие палитру 256 цветов.
newtype Gradient = Gradient (V.Vector Colour)
-- | разбор строки hex кода на подстроки по два символа (один байт)
-- и преобразование основного цвета из hex в 0..255
-- используется 'digitToInt'
hexParser :: String -> [Int]
hexParser (x:y:xs) = (digitToInt x)*16 + (digitToInt y) : (hexParser xs)
hexParser []       = []
hexParser _        = error "parser"
-- | преобразование основного цвета из 0..255 в [0,1]
byteToDouble :: Int -> Double
byteToDouble x = step * fromIntegral x
  where
    -- шаг в [0,1]
    step = 1 / 255
{- | преобразование основного цвета из String в Double
@
hexParser    :: String -> [Int]
byteToDouble :: Int -> Double
@
-}
hexToDouble :: String -> [Double]
hexToDouble s = map byteToDouble (hexParser s)

-- | сворачивание списка в кортежи по три компоненты
zipTo3 :: [Double] -> [Colour]
zipTo3 (x:y:z:lst) = (x,y,z) : (zipTo3 lst)
zipTo3 [] = []
zipTo3 _  = error "zipTo3"

-- | Преобразование палитры из 'String' в Gradient
paletteToDouble :: String -> Gradient
paletteToDouble s =
  Gradient (
    V.generate
    (length list)
    (\n -> list !! n)
           )
  where list = zipTo3 $ hexToDouble s
-- | Главная функция - Получение цвета по входящему Double
colorMap :: Gradient -> Double -> Colour
{-# INLINE colorMap #-}
colorMap (Gradient grad) pointer = grad V.! (round $ 255 * pointer)