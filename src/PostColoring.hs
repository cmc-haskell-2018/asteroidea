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
--import qualified Debug.Trace as Trace

--типы параметров обработки
type Width = Int                        -- ^ Ширина входящего изображения (до supersampling)
type Height = Int                       -- ^ Высота входящего изображения (до supersampling)
type KernelSize = Int                   -- ^ размер матрицы для матричного фильтра
type Kernel = (KernelSize -> [Double])  -- ^ матрица для матричного фильтра (по входящему размеру выдаёт матрицу в виде списка)
type SupersamplingScale = Int           -- ^ кратность уменьшения при суперсемплинге/размер матрицы выборки при суперсемплинг
type Gamma = Double                     -- ^ коэффициент гамма-коррекции
-- | все параметры обработки в одном типе
type PostColorParams = (Width, Height, KernelSize, Kernel, SupersamplingScale, Gamma)

type Bucket = [Cell]                    -- ^ подматрица, хранит окрестность пикселя в виде списка
type TempField = [Cell]                 -- ^ всё изображение, представленное в виде списка
type TempMatrix = Matrix.Matrix Cell    -- ^ всё изображение, представленное в виде матрицы

postColoring    -- ^ главная функция постобработки
    :: Model                    -- ^ модель изображения (имеющая необходимые параметры)
    -> Field                    -- ^ всё изображение, представленное в виде Field (вектор)
    -> (Field, Width, Height)   -- ^ (итоговое изображение, его ширина, его высота)
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

defaultKernelSize   -- ^ значение по умолчанию размера матрицы для матричного фильтра
    :: KernelSize
defaultKernelSize = 3

boxBlur      -- ^ значение по умолчанию матрицы для матричного фильтра
    :: Kernel
boxBlur scale = map ((*) (1 / fromIntegral (scale * scale))) [1..]

defaultScale -- ^ значение по умолчанию кратности уменьшения при суперсемплинг
    :: SupersamplingScale
defaultScale = 1

defaultGamma -- ^ значение по умолчанию коэффициента гамма-коррекции
    :: Gamma
defaultGamma = 2.2

fieldToTemp  -- ^ перевод Field -> TempField (перед функциями пост-обработки)
    :: Field
    -> TempField
fieldToTemp field = Vector.toList field

tempToField  -- ^ перевод TempField -> Field (после функций пост-обработки)
    :: TempField
    -> Field
tempToField field = Vector.fromList field

postColoringTemp    -- ^ главная функция постобработки, работающая с TempField
    :: PostColorParams  -- ^ все параметры постобработки, упакованные в специальный тип
    -> TempField        -- ^ входящее изображение, представленное в виде списка
    -> TempField        -- ^ получаемое изображение, представленное в виде списка
postColoringTemp params@(oldwidth, oldheight, ksize, kernel, scale, gamma) field = supersampling supersamplingParams $ kernelFilter kernelFilterParams field
    where
        kernelFilterParams = (oldwidth, oldheight, ksize, kernel)
        supersamplingParams = (oldwidth, oldheight, scale, gamma)


kernelFilter    -- ^ применение матричного фильтра (а точнее, подготовка изображения к его применению)
    :: (Width, Height, KernelSize, Kernel)  -- ^ (ширина, высота, размер матрицы и матрица матричного фильтра)
    -> TempField    -- ^ входящее изображение, представленное в виде списка
    -> TempField    -- ^ получаемое изображение, представленное в виде списка
kernelFilter params@(width, height, ksize, kernel) field | ksize == 1 = field
                                                         | otherwise = kernelFilterStep 0 (width, height, (kernel ksize), r) matrix
                                                            where
                                                                matrix = safeMatrix r (Matrix.fromList height width field)
                                                                r = div (ksize - 1) 2

-- | расширение матрицы по краям, чтобы после функция взятия
-- | окрестности работала для каждого элемента матрицы одинаково
safeMatrix  
    :: Int          -- ^ число строк/столбцов, которое нужно добавить к матрицы с каждой стороны
    -> TempMatrix   -- ^ изначальная матрица
    -> TempMatrix   -- ^ расширенная матрица
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

-- | результатом работы данной функции является функция,
-- | которая даёт элемент в новой матрице по его координатам в ней
generator
    :: Int                  -- ^ число строк/столбцов, на которое новая матрица расширенна
    -> TempMatrix           -- ^ старая матрица
    -> ((Int, Int) -> Cell) -- ^ функция, выдающая элемент новой матрицы по его координатам в ней
generator r matrix (row, col) = Matrix.unsafeGet oldRow oldCol matrix
    where
      oldRow = fromExtendedCoord rows r row
      oldCol = fromExtendedCoord cols r col
      rows = Matrix.nrows matrix
      cols = Matrix.ncols matrix

-- | шаг матричного фильтра для каждого
-- | элемента расширенной матрицы по его номеру.
-- | 
-- | на каждом шагу к пикселю применяется функция
-- | применения матричного фильтра
kernelFilterStep
    :: Int          -- ^ номер пикселя (если представить матрицу изображения списком)
    -> (Width, Height, [Double], Int) -- ^ (ширина и высота изображения, матрица матричного фильтра, расширение матрицы изображения (с одной стороны))
    -> TempMatrix   -- ^ расширенная матрица изображения
    -> TempField    -- ^ полученное изображение, представленное в виде списка
kernelFilterStep pixelNum params@(width, height, kernel, r) matrix | pixelNum == (width * height) = []
                                                                   | otherwise = applyFilter kernel (correspondingToPixel pixelNum width r matrix) : kernelFilterStep (pixelNum + 1) params matrix

-- | получение окрестности пикселя в расширенной матрице изображения
-- | 
-- | функция вызывает другую функцию, функцию
-- | извлечения подматрицы из любой произвольной матрицы,
-- | предварительно получая необходимые ей параметры
correspondingToPixel
    :: Int           -- ^ номер пикселя (если представить матрицу изображения списком)
    -> Width         -- ^ ширина изображения
    -> Int           -- ^ расширение матрицы изображения (с одной стороны)
    -> TempMatrix    -- ^ расширенная матрица изображения
    -> Bucket        -- ^ подматрица, хранящая окрестность изображения в виде списка
correspondingToPixel pixelNum width r matrix = getClosePixels startRow startColumn startRow endRow startColumn endColumn matrix
    where                
        startRow = centerY - r
        endRow = centerY + r
        startColumn = centerX - r
        endColumn = centerX + r
        centerX = r + 1 + mod pixelNum width
        centerY = r + 1 + div pixelNum width

getClosePixels -- ^ извлечения подматрицы из произвольной матрицы
    :: Int          -- ^ координата (в матрице) Y текущего элемента подматрицы
    -> Int          -- ^ координата (в матрице) X текущего элемента подматрицы
    -> Int          -- ^ координата (в матрице) Y верхнего левого элемента подматрицы
    -> Int          -- ^ координата (в матрице) Y нижнего правого элемента подматрицы
    -> Int          -- ^ координата (в матрице) X верхнего левого элемента подматрицы
    -> Int          -- ^ координата (в матрице) X нижнего правого элемента подматрицы
    -> TempMatrix   -- ^ матрица
    -> Bucket       -- ^ подматрица, представленная в виде списка
getClosePixels i j i1 i2 j1 j2 matrix | j == j2 = elem : []
                                      | i == i2 = elem : getClosePixels i1 (j + 1) i1 i2 j1 j2 matrix
                                      | otherwise = elem : getClosePixels (i + 1) j i1 i2 j1 j2 matrix
                                        where
                                            elem = Matrix.unsafeGet i j matrix

applyFilter -- ^ применение матричного фильтра к пикселю (окрестности пикселя)
    :: [Double] -- ^ матрица матричного фильтра
    -> Bucket   -- ^ окрестность пикселя в виде списка
    -> Cell     -- ^ итоговый пиксель
applyFilter kernel histogram = foldl1 foldOperation $ zipWith filterOperation kernel histogram

-- | умножение соответствующего элемента окрестности на
-- | соответствующий элемент матрицы матричного фильтра
filterOperation
    :: Double   -- ^ элемент матрицы матричного фильтра
    -> Cell     -- ^ элемент окрестности пикселя
    -> Cell     -- ^ результат умножения
filterOperation k (r, g, b, a) = (r * k, g * k, b * k, a * k)

foldOperation   -- ^ сложения результатов умножения (функции filterOperation)
    :: Cell     -- ^ слагаемое
    -> Cell     -- ^ слагаемое
    -> Cell     -- ^ сумма
foldOperation (r1, g1, b1, a1) (r2, g2, b2, a2) = (r1 + r2, g1 + g2, b1 + b2, a1 + a2)


supersampling   -- ^ функция применения суперсемплинга (подготовки к нему)
    :: (Width, Height, SupersamplingScale, Gamma) -- ^ (ширина и высота старого изображения, кратность уменьшения при суперсемплинг, коэффициент гамма-коррекции)
    -> TempField    -- ^ входящее изображение, представленное в виде списка
    -> TempField    -- ^ полученное изображение, представленное в виде списка
supersampling (oldwidth, oldheight, scale, gamma) field | scale == 1 = field
                                                        | otherwise = supersamplingChoosingHistogram 1 1 (currentwidth, currentheight, scale, gamma, maxalpha) matrix
                                                            where
                                                                maxalpha = getMaxAlpha field
                                                                matrix = Matrix.fromList oldheight oldwidth field
                                                                currentwidth = oldwidth - mod oldwidth scale
                                                                currentheight = oldheight - mod oldheight scale

getMaxAlpha -- ^ подсчет максимального альфа-канала во всём изображении
    :: TempField    -- ^ входящее изображение, представленное в виде списка
    -> Double       -- ^ max alpha
getMaxAlpha h = foldl1 max $ map (getComponent 3) h

-- | разбиение матрицы изображения на равные
-- | неперечекающиеся подматрицы/блоки (block-partition)
-- | 
-- | для каждого блока, находятся координаты его верхнего левого
-- | блока, после чего вызывается функция его извлечения
supersamplingChoosingHistogram
    :: Int          -- ^ номер по Y текущего блока
    -> Int          -- ^ номер по X текущего блока
    -> (Width, Height, SupersamplingScale, Gamma, Double) -- ^ (ширина и высота старого изображения, размер блока, коэффициент гамма-коррекции, max alpha)
    -> TempMatrix   -- ^ входящее изображение, представленное в виде матрицы
    -> TempField    -- ^ полученное изображение, представленное в виде списка
supersamplingChoosingHistogram i j params@(oldwidth, oldheight, scale, gamma, maxalpha) matrix | j > oldwidth = []
                                                                                               | i > oldheight = supersamplingChoosingHistogram 1 (j + scale) params matrix
                                                                                               | otherwise = elem : supersamplingChoosingHistogram (i + scale) j params matrix
                                                                                                where
                                                                                                    elem = getFinalCell (supersamplingGettingHistogram 0 0 i j scale matrix) maxalpha gamma

supersamplingGettingHistogram -- ^ извлечения блока из матрицы изображения
    :: Int                  -- ^ координата Y текущего пикселя в блоке
    -> Int                  -- ^ координата X текущего пикселя в блоке
    -> Int                  -- ^ номер по Y текущего блока в матрице
    -> Int                  -- ^ номер по X текущего блока в матрице
    -> SupersamplingScale   -- ^ размер блока
    -> TempMatrix           -- ^ входящее изображение, представленное в виде матрицы
    -> Bucket               -- ^ блок, представленный в виде списка
supersamplingGettingHistogram ishift jshift i j scale matrix | jshift == scale = []
                                                             | ishift == scale = supersamplingGettingHistogram 0 (jshift + 1) i j scale matrix
                                                             | otherwise = elem : supersamplingGettingHistogram (ishift + 1) jshift i j scale matrix
                                                                where
                                                                    elem = Matrix.unsafeGet (i + ishift) (j + jshift) matrix

-- | применение суперсэмплинга и гамма-коррекции
-- | к данному блоку
-- | 
-- | данная функция вызывает фильтрации для
-- | каждого канал и объединяет их
getFinalCell
    :: Bucket   -- ^ блок, представленный в виде списка
    -> Double   -- ^ максимальный альфа-канал во всём изображении
    -> Gamma    -- ^ коэффициент гамма-коррекции
    -> Cell     -- ^ итоговый пиксель
getFinalCell h maxalpha gamma = (r, g, b, a)
    where 
        r = getFinalColor 0 h maxalpha gamma
        g = getFinalColor 1 h maxalpha gamma
        b = getFinalColor 2 h maxalpha gamma
        a = getAvgComponent 3 h

getFinalColor   -- ^ final_pixel_color[x][y] := color_avg[x][y] * alpha[x][y]^(1/gamma)
    :: Int      -- ^ номер канала (r=0, g=1, b=2)
    -> Bucket   -- ^ блок, представленный в виде списка
    -> Double   -- ^ максимальный альфа-канал во всём изображении
    -> Gamma    -- ^ коэффициент гамма-коррекции
    -> Double   -- ^ итоговое значение канала
getFinalColor i h maxalpha gamma = (getAvgComponent i h) * ((getAlpha h maxalpha) ** (1 / gamma))

getAvgComponent -- ^ получение среднего значения канала в блоке
    :: Int      -- ^ номер канала (r=0, g=1, b=2, a=3)
    -> Bucket   -- ^ блок, представленный в виде списка
    -> Double
getAvgComponent i h = (foldl1 (+) $ map (getComponent i) h) / (fromIntegral $ length h)

getAlpha -- ^ alpha[x][y] := log10(avg[x][y]) / log10(max)
    :: Bucket   -- ^ блок, представленный в виде списка
    -> Double   -- ^ максимальный альфа-канал во всём изображении
    -> Double   -- ^ alpha[x][y]
getAlpha h maxalpha = (log $ getAvgComponent 3 h) / (log maxalpha)

getComponent -- ^ извлечение компонента(канала) из Cell
    :: Int      -- ^ номер канала (r=0, g=1, b=2, a=3)
    -> Cell     -- ^ пиксель
    -> Double   -- ^ значение канала
getComponent i (r, g, b, a) | i == 0 = r
                            | i == 1 = g
                            | i == 2 = b
                            | otherwise = a
