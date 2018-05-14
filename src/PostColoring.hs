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
import qualified Data.Matrix as Matrix
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
type TempMatrix = Matrix.Matrix Cell

-- | главная функция постобработки
postColoring :: PostColorParams -> Field -> Field
postColoring params field = tempToField (postColoringTemp params (fieldToTemp field))

boxBlur :: Scale -> [Double]
boxBlur scale = map ((*) (1 / fromIntegral (scale * scale))) [1..]

-- | перевод Field -> TempField
fieldToTemp :: Field -> TempField
fieldToTemp field = Vector.toList field

-- | перевод TempField -> Field
tempToField :: TempField -> Field
tempToField field = Vector.fromList field

-- | supersampling, работающая с TempField
postColoringTemp :: PostColorParams -> TempField -> TempField
postColoringTemp params field = supersampling params field


-- | разделение таблицы, хранящейся в списке, на полоски из клеток
-- |______
-- | aabb |  aabbaabbccddccdd 
-- | aabb |       ->
-- | ccdd |  aabbaabb ++ ccddccdd
-- | ccdd |
supersampling :: PostColorParams -> TempField -> TempField
supersampling _ [] = []
supersampling (oldwidth, oldheight, scale, gamma) field = divide ++ supersampling (oldwidth, oldheight, scale, gamma) remaining
    where
        (cut, remaining) = splitAt (oldwidth * scale) field
        divide = getHistograms (oldwidth, oldheight, scale, gamma) (supersampling2 (oldwidth, oldheight, scale, gamma) cut)

-- | aabbaabb -> [aabb, aabb]
supersampling2 :: PostColorParams -> TempField -> [TempField]
supersampling2 (oldwidth, oldheight, scale, gamma) lineOfHistograms = cut : supersampling2 (oldwidth, oldheight, scale, gamma) remaining
    where 
        (cut, remaining) = splitAt oldwidth lineOfHistograms

-- | [aabb, aabb] -> aaaabbbb -> AB (применение supersampling)
getHistograms :: PostColorParams -> [TempField] -> TempField
getHistograms _ [] = []
getHistograms (oldwidth, oldheight, scale, gamma) histograms = (getFinalCell (foldl1 (++) (map (take scale) histograms)) gamma) : (getHistograms (oldwidth, oldheight, scale, gamma) (map (drop scale) histograms))

-- | получение Cell из Field по x и y
getCell :: Int -> Field -> Int -> Int -> Cell
getCell width field i j = field Vector.! (i + j * width)

-- | извлечение компонента из Cell
getComponent :: Int -> Cell -> Double
getComponent i (r, g, b, a) | i == 0 = r
                            | i == 1 = g
                            | i == 2 = b
                            | otherwise = a

-- | avg[x][y]
getAvgComponent :: Int -> Histogram -> Double
getAvgComponent i h = (foldl1 (+) $ map (getComponent i) h) / (fromIntegral $ length h)

-- | max
getMaxComponent :: Int -> Histogram -> Double
getMaxComponent i h = foldl1 max $ map (getComponent i) h

-- | alpha[x][y] := log10(avg[x][y]) / log10(max)
getAlpha :: Histogram -> Double
getAlpha h = (log10 $ getAvgComponent 3 h) / (log10 $ getMaxComponent 3 h)

-- | final_pixel_color[x][y] := color_avg[x][y] * alpha[x][y]^(1/gamma)
getFinalColor :: Int -> Histogram -> Gamma -> Double
getFinalColor i h gamma = (getAvgComponent i h) * ((getAlpha h) ** (1 / gamma))

-- | объединение каналов
getFinalCell :: Histogram -> Gamma -> Cell
getFinalCell h gamma = (r, g, b, a)
    where
        r = getFinalColor 0 h gamma
        g = getFinalColor 1 h gamma
        b = getFinalColor 2 h gamma
        a = getAlpha h

-- | кол-во клеток по x
histogramsNumX :: OldWidth -> Scale -> Int
histogramsNumX oldwidth scale = floor (fromIntegral(oldwidth :: Int) / fromIntegral(scale :: Int))

-- | кол-во клеток по y
histogramsNumY :: OldHeight -> Scale -> Int
histogramsNumY oldheight scale = floor (fromIntegral(oldheight :: Int) / fromIntegral(scale :: Int))

log10 :: Double -> Double
log10 x = log x / log 10

grayscale :: Cell -> Double
grayscale (r, g, b, a) = 0.299 * r + 0.587 * g + 0.114 * b

-- | применение фильтра
filter :: Int -> Scale -> TempField -> TempField
filter = filterStep 0

-- | шаг фильтра для каждого элемента field по номеру
filterStep :: Int -> Int -> Scale -> TempField -> TempField
filterStep pixelNum width scale field | pixelNum == (length field) = []
                                      | otherwise = applyFilter (boxBlur scale) (correspondingToPixel pixelNum width scale field) : filterStep (pixelNum + 1) width scale field

-- | выделение окружающих пикселей в зависимости от scale
correspondingToPixel :: Int -> Int -> Scale -> TempField -> Histogram
correspondingToPixel pixelNum width scale field = Matrix.toList submatrix  
    where
        matrix = safeMatrix r (Matrix.fromList (div (length field) width) width field)
        submatrix = Matrix.submatrix startRow endRow startColumn endColumn matrix
        startRow = centerY - r
        endRow = centerY + r
        startColumn = centerX - r
        endColumn = centerX + r
        centerX = mod (pixelNum + 1) width
        centerY = div (pixelNum + 1) width
        r = div (scale - 1) 2

-- | решение проблемы об обработке концов изображения - отзеркаливание
safeMatrix :: Scale -> TempMatrix -> TempMatrix
safeMatrix r matrix = Matrix.matrix (2 * r + n0) (2 * r + m0) $ \(i,j) ->
    if i >= r && i <= n0 - r && j >= r && j <= m0 - r
        then Matrix.unsafeGet i j matrix
        else Matrix.unsafeGet (2 * n0 - i) (2 * m0 - j) matrix
    where
        n0 = Matrix.nrows matrix
        m0 = Matrix.ncols matrix

applyFilter :: [Double] -> Histogram -> Cell
applyFilter kernel histogram = foldl1 foldOperation $ zipWith filterOperation kernel histogram

filterOperation :: Double -> Cell -> Cell
filterOperation k (r, g, b, a) = (r * k, g * k, b * k, a * k)

foldOperation :: Cell -> Cell -> Cell
foldOperation (r1, g1, b1, a1) (r2, g2, b2, a2) = (r1 + r2, g1 + g2, b1 + b2, a1 + a2)

{--
takeThatAreClose :: Int -> Int -> Int -> Scale -> TempMatrix -> Histogram
takeThatAreClose currentNum pixelNum width scale field | currentNum == (length field) = []
                                                       | isClose = (getCell currentNum field) : next
                                                       | otherwise = next
    where
        isClose = distance <= maxDistance
        distance
        maxDistance = 
        next = takeThatAreClose (currentNum + 1) pixelNum width scale field
--}
-- | apply kernel
