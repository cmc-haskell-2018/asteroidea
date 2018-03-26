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
--import HexPalette
import Data.Char
--import Graphics.Gloss
--import GHC.Float
--type Gradient = 

type Color = (Float, Float, Float)
-- трёхкомпонентный цвет

type Palette = [Color]
--256 троек байт

currentPalette :: String
currentPalette = "A42D349C27319D222A9B1B269A1524980A2198082098061F98061E99071E97091FA00D1DB6141BBB151BC1171BCD1B1BDA1D1AE42019EF2418F32718F62B18F93117F93217F93318FA361AFB361CFC371EFC3A21F74625F54A26F44F28F3592CF2612FF26A33F37138F3783DF57D42F88641F78E42F69A41F5A745F3B248F3BB4CF3C24FF4C457F5C357F7C358F9C15AF9C058FBC45AFAC75CFACB61FAD166FAD56DFAD573FAD378F7CD7DF6C47FF7BE7DF7B679F7B076F7AB74F7A673F8A06FFA996AFA9064F9855EF97856F96A4CF95B43F94D3BF73F35F73130F6272BF21F27EC1925E81422E61020E50B1DE6081CE6081BE8081BE90719EA0818E80919E60B1AE50F1CE4131CE5181DE7211FEB2D22EF3925F54629F9532DFB6132FC6F3AFC7A40FC8145FC8648FC894AF9894CF3874DE8834EDE804ED9814ED8854FD88A53D99058DA9259DE9259E28F57E18A54DC8151D8774CD77045D76B42D96F44E1834AEB904DF69D51F8A353FBA956FAA855F9A755F29A4EF0944DEF8F4CEF8A4BEF854AEF8047F07C45F17B44F27A44F27340F0703FEE6E3EEE6B3EEF693FEF673FEF663FF45F3DF85C3CFC593BFC593CFC5A3EFA5B3EF85D3FEB603DDE5E3CC6583AB95138AD4B37A24336973C359538369435377C2B3176252E71202B7019256F13206F0F1D6F0C1B7306178003179305179C0A19A60F1BB2181EBE2221C42823CB2E26E34830EC5434F56039F76B3EFA7643FA7C45FB8347FB904EFC9F55FCBD65FCC96DFDD676FDDA79FDDF7DFDE684FDEA87FDED88FDEB85FDE982FAE57CF8E176F4DF73F0DD70EED46CEECB68EEB862EFB460F0B05EF0AD5DF1AB5CF6AC5AF0B059EEB25CEDB360EDB565EDB668EEB76CEFBC74F2BF7DFAC488FCCE91FCE4A2FCECA7FCF5ADFCF7AEFCF9AFFDFAB0FDF9AEFDF5A7FDF1A3FDEDA0FCEB9EFCEA9DFCE497FCDC90FDD289FDC581FDAB6DFDA467FD9D62FE8D59FD8053FE754DFD6A48FE5542FD4E3EFD473BFD4439FD4138FE3D36FE3935FE3834FE3833FC3833FC3832FC3832FB3833F63933EB3834DF3836C73637BB3436AF3236A92F35"
-- текущая палитра

step :: Float
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

byteToPrimary :: Int -> Float
byteToPrimary x = step * fromIntegral x
-- преобразование основного цвета из 0..255 в [0,1]

hexToPrimary :: String -> Float
hexToPrimary s = byteToPrimary (hexToByte s)
-- преобразование основного цвета из String в Float

hexToColor :: String -> Color
hexToColor s = ( hexToPrimary $ take 2 s , hexToPrimary . take 2 $ drop 2 s , hexToPrimary $ drop 4 s )
-- преобразование цвета из String в (Float, Float, Float)

paletteToDouble :: String -> Palette
paletteToDouble [] = []
paletteToDouble s = (hexToColor . take 6 $ s) : (paletteToDouble . drop 6 $ s)
-- Преобазование палитры из String в [(Float, Float, Float)]

getColor :: Palette -> Int -> Color
getColor (xs:s) 0 = xs
getColor (xs:s) n = getColor s (n - 1)
-- выбор цвета с нужным номером из палитры

getPalette :: Palette
getPalette = paletteToDouble currentPalette
-- передача текущей палитры в подходящем формате [(Float, Float, Float)]

getNumber :: Double -> Int
getNumber d = round $ 255 * d
-- Преобразование [0,1] в 0..255

colorMap :: Double -> (Float, Float, Float)
colorMap d = getColor getPalette $ getNumber d
-- Главная фунцкия - Получение цвета по входящему Double

-- проверка
{--
mapTuple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple f (a1, a2, a3) = (f a1, f a2, f a3)

test :: Double -> (Int, Int, Int)
test d = mapTuple (\x -> round $ 255 * x) (colorMap d)
--}
