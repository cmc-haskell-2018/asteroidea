{-|
Module      : Gradient
Description : operations with gradients, generating invocation to Model
Copyright   : Just Nothing
Stability   : OLEG
-}
{--
Для базы необходимо лишь нормальное представление градиента (вектор или матрица) и доступ к цветам в нем по Double [0,1]
Далее понадобятся функции для циклического сдвига, изменения hue\saturation\brightness и прочие ништяки
--}
module Gradient where
--import HexGradient
import Data.Char
--import Graphics.Gloss
--import GHC.Float
--type Gradient = 

type Color = (Double, Double, Double)
-- трёхкомпонентный цвет

type Gradient = [Color]
--256 троек байт

step :: Double
step = 1 / 255
-- шаг в [0,1]

charToInt :: Char -> Int
charToInt c | elem c ['0'..'9'] = ord c - ord '0'
            | elem c ['A'..'F'] = ord c - ord 'A' + 10
-- преобразование шестнадцатеричное число в десятичное

hexToByte :: String -> Int
hexToByte [] = 0
hexToByte (s1:s2:empty) = charToInt s1 * 16 + charToInt s2
-- преобразование основного цвета из hex в 0..255

byteToPrimary :: Int -> Double
byteToPrimary x = step * fromIntegral x
-- преобразование основного цвета из 0..255 в [0,1]

hexToPrimary :: String -> Double
hexToPrimary s = byteToPrimary (hexToByte s)
-- преобразование основного цвета из String в Float

hexToColor :: String -> Color
hexToColor s = ( hexToPrimary $ take 2 s , hexToPrimary . take 2 $ drop 2 s , hexToPrimary $ drop 4 s )
-- преобразование цвета из String в (Float, Float, Float)

paletteToDouble :: String -> Gradient
paletteToDouble [] = []
paletteToDouble s = (hexToColor . take 6 $ s) : (paletteToDouble . drop 6 $ s)
-- Преобазование палитры из String в [(Float, Float, Float)]

getColor :: Gradient -> Int -> Color
getColor (xs:s) 0 = xs
getColor (xs:s) n = getColor s (n - 1)
-- выбор цвета с нужным номером из палитры
getNumber :: Double -> Int
getNumber d = round $ 255 * d
-- Преобразование [0,1] в 0..255

colorMap :: Gradient -> Double -> Color
colorMap g d = getColor g $ getNumber d
-- Главная фунцкия - Получение цвета по входящему Double