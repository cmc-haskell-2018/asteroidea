{-|
Module      : Types
Description : declarating types, implementation of main data, etc
Copyright   : Just Nothing
Stability   : Stable
-}
module Types (module Types, module GVector, module Matrix) where
import Prelude
import qualified Data.Vector.Unboxed as Vector
import GVector
import Gradient
import Matrix

-- | Точка и цвет в карте градиентов [0,1)
type Cast = (Vec, Double)

-- | Бросок , цвет , номер трансформы
type CastGen = (GVec,Double,Transform)
-- МБ, переформировать как Data.Vector.Mutable.Vector
-- из Cell == Unbox (,,,) ?
-- | Ячейка поля - четыре вещественных значения, RGB и counter
type Cell = (Double,Double,Double,Double)
-- | Поле, двумерный (в нашем воображении)
-- распакованный (по вектору для каждой компоненты Cell)
-- вектор значений
type Field = Vector.Vector Cell
-- | перевод из двумерных координат в линейное смещение внутри структуры поля
linearFieldIndex :: Model -> (Int, Int) -> Int
linearFieldIndex m (i, j) = i + j * (mWidth m)
{-# INLINE linearFieldIndex #-}

-- МБ, оформить как Rand -> (Vec -> Vec) ?
-- Тогда можно сгенерировать отдельно список трансформ, каждая из которых возьмёт генератор и даст отображение (Vec -> Vec) плюс (Col -> Col).
-- И проход по линии 256 бросков сведётся к генерации трансформ,
-- и свёртке этих функций ( (Vec -> Vec) , (Col -> Col) ).
-- Генерацию трансформ можо вести быстро и легко,
-- ввести в рандом выдачу нескольких значений (остатков),
-- и будут одновременно выдаваться и номер трансформы,
-- и случайное зерно для вариации.
-- | Вариация как она есть - перевод GVec -> GVec
type Variation = (GVec -> GVec)

{- | Преобразование точки, цвета и всего такого
По сути - Transform олицетворяет отображение
из старой точки и цвета в новые точку и цвет.
Обычно такое отображение точек - это сложное выражение из функций-вариаций
Это отображение обладает влиянием на:
вероятностное распределение,
конечное размещение точки в поле
-}
data Transform = Transform {
-- | возможны линейные комбинации, композиция, параметры
tVariation :: Variation,
-- | вес в вероятностном распределении
tWeight :: Double,
-- | калибровка коэффициентов при смешении цветов
tColorPosition, tColorSpeed :: Double,
-- | яркость - мера влияния трансформы на точку конечного поля
tOpacity :: Double,
-- | xaos - переопределение вероятностного пространства
-- в терминах данной трансформы
tXaos :: [Double]
}
-- | шаблон модели
templateTransform :: Transform
{-# INLINABLE templateTransform #-}
templateTransform = Transform {
                 tVariation     = id
               , tWeight        = 1
               , tColorPosition = 0
               , tColorSpeed    = 0
               , tOpacity       = 1
               , tXaos          = []
               }

-- | Набор параметров, однозначно задающих фрактал
data Model = Model {
  -- | имя модели, используется в разборе аргументов запуска
  mName :: String,
  -- | череда трансформ
  mTransforms :: [Transform],
  -- | финальная трансформа, применяемая после выбранной, но до камеры
  mFinal :: Maybe Transform,
  -- | карта градиентов
  mGradient :: Gradient,
  -- | размер картинки
  mWidth, mHeight :: Int,
  -- КМК, лучше сделать отображением типа аффинной матрицы
  -- | Камера: зум-фактор, смещение и поворот.
  mScale, mShiftX, mShiftY, mRotation  :: Double,
  -- | Фоновый цвет
  mBackgroundColour :: (Int -> Cell),
  -- | параметры счёта
  mOuterIter,mInnerIter :: Int
}
-- | Шаблон модели
templateModel :: Model
{-# INLINABLE templateModel #-}
templateModel = Model {
    mName             = "42"
  , mTransforms       = []
  , mFinal           = Nothing
  , mGradient         = grad
  , mWidth            = 1024
  , mHeight           = 1024
  , mScale            = 1
  , mShiftX           = 0
  , mShiftY           = 0
  , mRotation         = 0
  , mBackgroundColour = (\_->(0,0,0,1))
  , mOuterIter        = 21845
  , mInnerIter        = 256
}
  where grad = paletteToDouble "\
\AA3B1CAA3F1C8144254454254D6D2551792F51712F5D713865854B758D5589A1\
\5E8DA254C2AD41DA8B2FDE7725D6672FD66A25C25F1C914C254C692534501220\
\4409203C122030122038122C48124069256189388DB25EAAC67AEED17AFAE154\
\E6972EE67A25E67A25DB6925CA5E25B64C1C853712542B09241B091C20001C30\
\09204009405C1C54793869914B91AE6796B27195AE718DAA71718D55516D4240\
\442F24341C1018120C0C090408000004000004000004000000000400000C0000\
\100B00240F00441712812612AA3712C25F1CB27C2F9AAE67A6C27AAACA7AB2D2\
\8DBEDAA0BED297CAD697E2DE84B6DA8DB6D28DBACE8DBECE97C6CEA9D2DAB3EA\
\E6C5EAEAC5FAF1BCF6E5BCEAC08DD2AC96BECA96B2C697B6CE8DB6CA8DB6C684\
\AACA84A6C6849EBA7A8DB27171954B5175383C542530401C1838091428091418\
\090C1009070C090408090804091407091C0F09241709502712812F12B23B12C6\
\5725DB6625DE6625DE751CE2871CE68E1CF29225FA9B1CF6BB1CEA7F2FFABC67\
\FAE170FAE154FED32EFECF2EFED04ADAB55E957867597942405D2F203C12142C\
\09041800000C0004080000040000040000040004000004000004040004040004\
\0800040800040800000400000000000009000000000000000000000000000000\
\0000000400000404000C0800101000201000381B095933097D4309A63F12B248\
\12C65A1CBE6B259D703899632F59793855653871481CB65025CE6225D26625D6\
\6E1CD66E1CDA7625DE6E1CE27225DE6E2FEE6F41C6A054A99470A1BA7AAAC284\
\BECE97CED6A0E2EEBCF6EEC5FEFAFEFAFECEF2FECEE6EEC5C6D6A0AAC2849195\
\71546D423840251C2C12182409141409201309300F09652712892F12A6331391\
\3712652F124427122038091C2C09141C09101409081000081000040C00041000\
\081409102009143012284812446D2561813889AA6795B67191B67181A15E6985\
\4B4459383C4C253040253C441C713712A63313BE4C1CCA5B1CD25E1CD2621CC6\
\57129E37096922002C0F001404000C00000804000408000810001020091C3409"