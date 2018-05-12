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
--import qualified Data.Tuple.Select as Select

-- |...
--параметр обработки
type OldWidth = Int
type OldHeight = Int
type Scale = Int
type Gamma = Double
-- | все параметры обработки
type PostColorParams = (OldWidth, OldHeight, Scale, Gamma)

type Histogram = [Cell]

type TempField = [Cell]

histogramsNumX :: OldWidth -> Scale -> Int
histogramsNumX oldwidth scale = floor (fromIntegral(oldwidth :: Int) / fromIntegral(scale :: Int))

histogramsNumY :: OldHeight -> Scale -> Int
histogramsNumY oldheight scale = floor (fromIntegral(oldheight :: Int) / fromIntegral(scale :: Int))

log10 :: Double -> Double
log10 x = log x / log 10


-- | главная функция постобработки
postColoring :: PostColorParams -> TempField -> TempField
postColoring _ [] = []
postColoring (oldwidth, oldheight, scale, gamma) field = divide ++ postColoring (oldwidth, oldheight, scale, gamma) remaining
    where
        (cut, remaining) = splitAt (oldwidth * scale) field
        divide = getHistograms (oldwidth, oldheight, scale, gamma) (postColoringStep2 (oldwidth, oldheight, scale, gamma) cut)

postColoringStep2 :: PostColorParams -> TempField -> [TempField]
postColoringStep2 (oldwidth, oldheight, scale, gamma) lineOfHistograms = cut : postColoringStep2 (oldwidth, oldheight, scale, gamma) remaining
    where 
        (cut, remaining) = splitAt oldwidth lineOfHistograms

getHistograms :: PostColorParams -> [TempField] -> TempField
getHistograms _ [] = []
getHistograms (oldwidth, oldheight, scale, gamma) histograms = (getFinalCell (foldl1 (++) (map (take scale) histograms)) gamma) : (getHistograms (oldwidth, oldheight, scale, gamma) (map (drop scale) histograms))

getCell :: Int -> Field -> Int -> Int -> Cell
getCell width field i j = field Vector.! (i + j * width)

getComponent :: Int -> Cell -> Double
getComponent i (r, g, b, a) | i == 0 = r
                            | i == 1 = g
                            | i == 2 = b
                            | otherwise = a

getAvgComponent :: Int -> Histogram -> Double
getAvgComponent i h = (foldl1 (+) $ map (getComponent i) h) / (fromIntegral $ length h)

getMaxComponent :: Int -> Histogram -> Double
getMaxComponent i h = foldl1 max $ map (getComponent i) h

getAlpha :: Histogram -> Double
getAlpha h = (log10 $ getAvgComponent 3 h) / (log10 $ getMaxComponent 3 h)

getFinalColor :: Int -> Histogram -> Gamma -> Double
getFinalColor i h gamma = (getAvgComponent i h) * ((getAlpha h) ** (1 / gamma))

getFinalCell :: Histogram -> Gamma -> Cell
getFinalCell h gamma = (r, g, b, a)
    where
        r = getFinalColor 0 h gamma
        g = getFinalColor 1 h gamma
        b = getFinalColor 2 h gamma
        a = getAlpha h
