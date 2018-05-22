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
import qualified GHC.Conc.Sync as Sync
--import qualified Data.Tuple.Select as Select
import qualified Debug.Trace as Trace

-- |...
--параметр обработки
type Width = Int
type Height = Int
type KernelSize = Int
type Kernel = (KernelSize -> [Double])
type SupersamplingScale = Int
type Gamma = Double
-- | все параметры обработки
type PostColorParams = (Width, Height, KernelSize, Kernel, SupersamplingScale, Gamma)

type Bucket = [Cell]
type TempField = [Cell]
type TempMatrix = Matrix.Matrix Cell

-- | главная функция постобработки
postColoring :: Model -> Field -> (Field, Width, Height)
postColoring model field = (tempToField (postColoringTemp params (fieldToTemp field)), newwidth, newheight)
    where
        params = (oldwidth, oldheight, kernelsize, kernel, scale, gamma)
        oldwidth = mWidth model
        oldheight = mHeight model
        kernelsize = defaultKernelSize
        kernel = boxBlur
        scale = defaultScale
        gamma = defaultGamma
        newwidth = div oldwidth scale
        newheight = div oldheight scale

defaultKernelSize :: KernelSize
defaultKernelSize = 3

boxBlur :: KernelSize -> [Double]
boxBlur scale = map ((*) (1 / fromIntegral (scale * scale))) [1..]
-- | boxBlur scale = [0..]
-- | boxBlur scale = Matrix.toList (Matrix.setElem (center, center) (Matrix.zero scale scale))
-- |    where
-- |        center = div (scale + 1) 2

defaultScale :: SupersamplingScale
defaultScale = 1

defaultGamma :: Gamma
defaultGamma = 2.2

-- | перевод Field -> TempField
fieldToTemp :: Field -> TempField
fieldToTemp field = Vector.toList field

-- | перевод TempField -> Field
tempToField :: TempField -> Field
tempToField field = Vector.fromList field

-- | supersampling, работающая с TempField
postColoringTemp :: PostColorParams -> TempField -> TempField
postColoringTemp params@(oldwidth, oldheight, ksize, kernel, scale, gamma) field = supersampling supersamplingParams $ kernelFilter kernelFilterParams field
    where
        kernelFilterParams = (oldwidth, oldheight, ksize, kernel)
        supersamplingParams = (oldwidth, oldheight, scale, gamma)


-- | применение фильтра
kernelFilter :: (Width, Height, KernelSize, Kernel) -> TempField -> TempField
kernelFilter params@(width, height, ksize, kernel) field | ksize == 1 = field
                                                         | otherwise = kernelFilterStep 0 (width, height, (kernel ksize), r) matrix
                                                            where
                                                                matrix = safeMatrix r (Matrix.fromList height width field)
                                                                r = div (ksize - 1) 2

-- | решение проблемы об обработке концов изображения - отзеркаливание
safeMatrix :: Int -> TempMatrix -> TempMatrix
safeMatrix r matrix = Matrix.matrix (2 * r + rows) (2 * r + cols) (generator r matrix)
    where
        rows = Matrix.nrows matrix
        cols = Matrix.ncols matrix


-- | Преобразование координаты ячейки расширенной матрицы
-- в координату ячейки исходной матрицы.
--
-- Ячейки в центре расширенной матрицы соответствуют
-- ячейкам исходной матрицы.
--
-- Ячейки по краям расширенной матрицы соответствуют
-- ячейкам зеркальной версии исходной матрицы.
--
-- TODO: пример.
--
-- >>> map (fromExtendedCoord 10 3) [1..16]
-- [3,2,1,1,2,3,4,5,6,7,8,9,10,10,9,8]
fromExtendedCoord
  :: Int  -- ^ Размер исходной матрицы (по одной координате).
  -> Int  -- ^ Расширение матрицы (с одной стороны).
  -> Int  -- ^ Координата в расширенной матрице.
  -> Int  -- ^ Координата в исходной матрице.
fromExtendedCoord n r i
  | i <= r      = r - i + 1          -- левый/верхний край
  | i > n + r   = 2 * n + r - i + 1  -- правый/нижний край
  | otherwise   = i - r              -- середина

generator :: Int -> TempMatrix -> ((Int, Int) -> Cell)
generator r matrix (row, col) = Matrix.unsafeGet oldRow oldCol matrix
    where
      oldRow = fromExtendedCoord rows r row
      oldCol = fromExtendedCoord cols r col

      rows = Matrix.nrows matrix
      cols = Matrix.ncols matrix

-- | шаг фильтра для каждого элемента matrix по номеру
kernelFilterStep :: Int -> (Width, Height, [Double], Int) -> TempMatrix -> TempField
kernelFilterStep pixelNum params@(width, height, kernel, r) matrix | pixelNum == (width * height) = []
                                                                   | otherwise = applyFilter kernel (correspondingToPixel pixelNum width r matrix) : kernelFilterStep (pixelNum + 1) params matrix
{--                                                                | otherwise = a `Sync.par` b `Sync.pseq` a : b
                                                                        where
                                                                            a = applyFilter (boxBlur scale) (correspondingToPixel pixelNum width r matrix)
                                                                            b = kernelFilterStep (pixelNum + 1) matrix width height scale r--}

-- | выделение окружающих пикселей в зависимости от scale
correspondingToPixel :: Int -> Width -> Int -> TempMatrix -> Bucket
correspondingToPixel pixelNum width r matrix = {--Trace.trace (show pixelNum)--} (getClosePixels startRow startColumn startRow endRow startColumn endColumn matrix)
    where                
        startRow = centerY - r
        endRow = centerY + r
        startColumn = centerX - r
        endColumn = centerX + r
        centerX = r + 1 + mod pixelNum width
        centerY = r + 1 + div pixelNum width

getClosePixels :: Int -> Int -> Int -> Int -> Int -> Int -> TempMatrix -> Bucket
getClosePixels i j i1 i2 j1 j2 matrix | j == j2 = elem : []
                                      | i == i2 = elem : getClosePixels i1 (j + 1) i1 i2 j1 j2 matrix
                                      | otherwise = elem : getClosePixels (i + 1) j i1 i2 j1 j2 matrix
                                        where
                                            elem = Matrix.unsafeGet i j matrix

applyFilter :: [Double] -> Bucket -> Cell
applyFilter kernel histogram = foldl1 foldOperation $ zipWith filterOperation kernel histogram

filterOperation :: Double -> Cell -> Cell
filterOperation k (r, g, b, a) = (r * k, g * k, b * k, a * k)

foldOperation :: Cell -> Cell -> Cell
foldOperation (r1, g1, b1, a1) (r2, g2, b2, a2) = (r1 + r2, g1 + g2, b1 + b2, a1 + a2)


supersampling :: (Width, Height, SupersamplingScale, Gamma) -> TempField -> TempField
supersampling (oldwidth, oldheight, scale, gamma) field | scale == 1 = field
                                                        | otherwise = supersamplingChoosingHistogram 1 1 (currentwidth, currentheight, scale, gamma, maxalpha) matrix
                                                            where
                                                                maxalpha = getMaxAlpha field
                                                                matrix = Matrix.fromList oldheight oldwidth field
                                                                currentwidth = oldwidth - mod oldwidth scale
                                                                currentheight = oldheight - mod oldheight scale

-- | max alpha
getMaxAlpha :: Bucket -> Double
getMaxAlpha h = foldl1 max $ map (getComponent 3) h

supersamplingChoosingHistogram :: Int -> Int -> (Width, Height, SupersamplingScale, Gamma, Double) -> TempMatrix -> TempField
supersamplingChoosingHistogram i j params@(oldwidth, oldheight, scale, gamma, maxalpha) matrix | j > oldwidth = []
                                                                                               | i > oldheight = {--Trace.trace (show j)--} (supersamplingChoosingHistogram 1 (j + scale) params matrix)
                                                                                               | otherwise = elem : supersamplingChoosingHistogram (i + scale) j params matrix
                                                                                                where
                                                                                                    elem = getFinalCell (supersamplingGettingHistogram 0 0 i j scale matrix) maxalpha gamma

supersamplingGettingHistogram :: Int -> Int -> Int -> Int -> SupersamplingScale -> TempMatrix -> Bucket
supersamplingGettingHistogram ishift jshift i j scale matrix | jshift == scale = []
                                                             | ishift == scale = supersamplingGettingHistogram 0 (jshift + 1) i j scale matrix
                                                             | otherwise = elem : supersamplingGettingHistogram (ishift + 1) jshift i j scale matrix
                                                                where
                                                                    elem = Matrix.unsafeGet (i + ishift) (j + jshift) matrix

-- | объединение каналов
getFinalCell :: Bucket -> Double -> Gamma -> Cell
getFinalCell h maxalpha gamma = (r, g, b, a)
    where 
        r = getFinalColor 0 h maxalpha gamma
        g = getFinalColor 1 h maxalpha gamma
        b = getFinalColor 2 h maxalpha gamma
        a = getAvgComponent 3 h

-- | final_pixel_color[x][y] := color_avg[x][y] * alpha[x][y]^(1/gamma)
getFinalColor :: Int -> Bucket -> Double -> Gamma -> Double
getFinalColor i h maxalpha gamma = (getAvgComponent i h) * ((getAlpha h maxalpha) ** (1 / gamma))

-- | avg[x][y]
getAvgComponent :: Int -> Bucket -> Double
getAvgComponent i h = (foldl1 (+) $ map (getComponent i) h) / (fromIntegral $ length h)

-- | alpha[x][y] := log10(avg[x][y]) / log10(max)
getAlpha :: Bucket -> Double -> Double
getAlpha h maxalpha = (log10 $ getAvgComponent 3 h) / (log10 maxalpha {--$ getMaxComponent 3 h-})

-- | извлечение компонента из Cell
getComponent :: Int -> Cell -> Double
getComponent i (r, g, b, a) | i == 0 = r
                            | i == 1 = g
                            | i == 2 = b
                            | otherwise = a

log10 :: Double -> Double
log10 x = log x / log 10

{--
getClosePixels :: Int -> Int -> Int -> Int -> Int -> Int -> TempMatrix -> Histogram
getClosePixels i j i1 i2 j1 j2 matrix | j == j2 = elem : []
                                      | i == i2 = elem : getClosePixels i1 (j + 1) i1 i2 j1 j2 matrix
                                      | otherwise = elem : getClosePixels (i + 1) j i1 i2 j1 j2 matrix
                                        where
                                            elem = Matrix.unsafeGet i j matrix
--}
-- | разделение таблицы, хранящейся в списке, на полоски из клеток
-- |______
-- | aabb |  aabbaabbccddccdd 
-- | aabb |       ->
-- | ccdd |  aabbaabb ++ ccddccdd
-- | ccdd |
{--
supersamplingOld :: PostColorParams -> TempField -> TempField
supersamplingOld _ [] = []
supersamplingOld params@(oldwidth, oldheight, scale, gamma) field = divide ++ supersamplingOld params remaining
    where
        (cut, remaining) = splitAt (oldwidth * scale) field
        divide = getHistograms params (supersampling2 params cut)

-- | aabbaabb -> [aabb, aabb]
supersampling2 :: PostColorParams -> TempField -> [TempField]
supersampling2 params@(oldwidth, oldheight, scale, gamma) lineOfHistograms = cut : supersampling2 params remaining
    where 
        (cut, remaining) = splitAt oldwidth lineOfHistograms

-- | [aabb, aabb] -> aaaabbbb -> AB (применение supersamplingOld)
getHistograms :: PostColorParams -> [TempField] -> TempField
getHistograms _ [] = []
getHistograms params@(oldwidth, oldheight, scale, gamma) histograms = (getFinalCell (foldl1 (++) (map (take scale) histograms)) gamma) : (getHistograms params (map (drop scale) histograms))
--}

{--
gammaFilter :: Gamma -> TempField -> TempField
gammaFilter gamma field = gammaFilterStep gamma field field

gammaFilterStep :: Gamma -> TempField -> TempField -> TempField
gammaFilterStep _ _ [] = []
gammaFilterStep gamma h (f@(r, g, b, a) : fs) = (nr, ng, nb, a) : gammaFilterStep gamma h fs
    where
        nr = r * getFinalColor 0 h gamma
        ng = g * getFinalColor 1 h gamma
        nb = b * getFinalColor 2 h gamma

-- | получение Cell из Field по x и y
getCell :: Int -> Field -> Int -> Int -> Cell
getCell width field i j = field Vector.! (i + j * width)
--}

        {--
          r = (getAvgComponent 0 h)
        g = getAvgComponent 1 h
        b = getAvgComponent 2 h
        a = getAvgComponent 3 h
        --}

{--
-- | max
getMaxComponent :: Int -> Histogram -> Double
getMaxComponent i h = foldl1 max $ map (getComponent i) h


-- | final_pixel_color[x][y] := color_avg[x][y] * alpha[x][y]^(1/gamma)
getFinalColor :: Int -> Histogram -> Double -> Gamma -> Double
getFinalColor i h maxalpha gamma = (getAvgComponent i h) * ((getAlpha h maxalpha) ** (1 / gamma))
--}

{--
-- | кол-во клеток по x
histogramsNumX :: Width -> Scale -> Int
histogramsNumX oldwidth scale = floor (fromIntegral(oldwidth :: Int) / fromIntegral(scale :: Int))

-- | кол-во клеток по y
histogramsNumY :: Height -> Scale -> Int
histogramsNumY oldheight scale = floor (fromIntegral(oldheight :: Int) / fromIntegral(scale :: Int))

grayscale :: Cell -> Double
grayscale (r, g, b, a) = 0.299 * r + 0.587 * g + 0.114 * b

--}

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
