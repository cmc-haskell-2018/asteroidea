module Asteroidea where

import Graphics.Gloss
import ClassField
import System.Random
import Const
import Data.Matrix
--import Data.ByteString

run :: IO ()
--run = putStrLn "Just Nothing"
run = do
  genRand <- newStdGen
  simulate display color fps initField imageScan (updateField genRand) 
  where
    display = InWindow "Just Nothing" (sizeX, sizeY) (startPosX, startPosY)
    -- FullScreen
    color = backGrCol
    fps = fpsMax
    initField :: Field
    initField = createField sizeX sizeY
    imageScan :: Field -> Picture
    imageScan _  = blank
    -- | Just Order
{-- deprecated
    imageScan field =
      bitmapOfByteString 
      pack (
        (foldr (++) []) .
        (map 
          (
          ((\f (a,b,c,d) -> [f a,f b,f c,f d] )
          (fromRational . (*255))) .
          rgbaOfColor
          )
        (toList field)
        )
      )
      True
--}
{--
    imageScan field =
      pictures (concat
      [[ Color (unsafeGet i j field) (Line [(fromIntegral i,fromIntegral j)]) | i<- [1..sizeX]] | j <- [1..sizeY]])
--}
    --updField :: genRand -> ViewPort -> Float -> Field -> Field
    --updField g _ dt = updateField 