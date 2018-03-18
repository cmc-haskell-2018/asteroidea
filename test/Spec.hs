main :: IO ()
main = putStrLn "Test suite not yet implemented"

{--
import System.Random
import Data.Matrix
:{
let 
  gen = mkStdGen 42
  field = createField sizeX sizeY
  pack newC@(cast, _) = ((plot cast field), newC)
  ctrl ((point,col),gen) = control point
  castgen = busPoint (mkStdGen 42) 8
  cast = fst castgen
  temp n = fst (busPoint (mkStdGen 42) n)
  trr size = truncate . (+ ((fromIntegral size)/2))
  colour ((ordX, ordY), colC) = merge colC $ getPoint (coord ((ordX, ordY), colC))
  getPoint (a,b) = getElem a b field
  flag ((ordX, ordY), colC) = control (ordX, ordY)
  coord ((ordX, ordY), colC) = ((trr sizeX) ordX, (trr sizeY) ordY)
:}
--}