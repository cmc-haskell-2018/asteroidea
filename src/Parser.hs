{-# OPTIONS_GHC -w #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Parser
Description : file to model
Copyright   : Just Nothing
Stability   : ARTEM
-}
module Parser where
import Types 
import Variations	
import Gradient
import Data.List.Split
import Data.List


parseModel :: String -> Model -> Model
parseModel file mod 
    | (elem "tXaos" (words file)) = checkModel $ parseModel' (words file) mod 
    | otherwise = parseModel' (words file) mod
    
-- | check the xaos arguments
checkModel :: Model -> Model
checkModel mod
    | and $ map (== ( length $ mTransforms mod)) $ map ( length . tXaos) $ mTransforms mod = mod
    | otherwise = error "wrong number of args in Xaos"

-- | read a model from the file
parseModel' :: [String] -> Model -> Model
parseModel' (fName : fVal : rest) mod 
    | (fName == "mName")             = parseModel' rest mod {mName = fVal}
    | (fName == "mTransforms")       = let 
                                       l = (splitOn ["endT"] (fVal : rest))
                                       trans = map (parseTran templateTransform) (init l)
                                       r = last l 
    								   in parseModel' r mod {mTransforms = trans}
    | (fName == "mFinal")            = let (t:r:[]) = (splitOn ["endF"] (fVal : rest)) 
									   in parseModel' r mod {mFinal = Just (parseTran templateTransform t )}
    | (fName == "mGradient")         = parseModel' rest mod {mGradient = paletteToDouble fVal}
    | (fName == "mWidth")            = parseModel' rest mod {mWidth = read fVal :: Int}
    | (fName == "mHeight")           = parseModel' rest mod {mHeight = read fVal :: Int}
    | (fName == "mScale")            = parseModel' rest mod {mScale = read fVal :: Double}
    | (fName == "mShiftX")           = parseModel' rest mod {mShiftX = read fVal :: Double}
    | (fName == "mShiftY")           = parseModel' rest mod {mShiftY = read fVal :: Double}
    | (fName == "mRotation")         = parseModel' rest mod {mRotation = read fVal :: Double}
    | (fName == "mBackgroundColour") = parseModel' (drop 2 rest) mod {mBackgroundColour = (\_ -> (read fVal :: Double, read (head rest) :: Double, read (head $ tail $ rest) :: Double, 1))}
    | (fName == "mOuterIter")        = parseModel' rest mod {mOuterIter = read fVal :: Int}
    | (fName == "mInnerIter")        = parseModel' rest mod {mInnerIter = read fVal :: Int}
    | otherwise                      = error ("Error, no such argument in model: " ++ fName)
parseModel' _ mod = mod

tranDelims :: [String]
tranDelims = ["tVariation", "tWeight", "tColorPosition", "tColorSpeed", "tOpacity", "tXaos"]

-- | parse a single transform
parseTran :: Transform -> [String] -> Transform
parseTran tran (fName : fVal : rest)
     | (fName == "tVariation")     = let (var, remains) = span (\x -> not $ elem x tranDelims) (fVal : rest)
                                          in parseTran tran {tVariation = parseVar $ intercalate " " var} remains 
  	| (fName == "tWeight")        = parseTran tran {tWeight = read fVal :: Double} rest
  	| (fName == "tColorPosition") = parseTran tran {tColorPosition = read fVal :: Double} rest
  	| (fName == "tColorSpeed")    = parseTran tran {tColorSpeed = read fVal :: Double} rest
  	| (fName == "tOpacity")       = parseTran tran {tOpacity = read fVal :: Double} rest
  	| (fName == "tXaos")          = let (xaos, remains) = span (\x -> not $ elem x tranDelims) (fVal : rest)
  						            in parseTran tran {tXaos = parseXaos xaos} remains 
     | otherwise = error ("Error, no such argument in transform: " ++ fName)
parseTran tran _ = tran

parseXaos :: [String] -> [Double]
parseXaos str = map (read) str

getVar :: [String] -> Variation
getVar (fName:pars) 
    | (fName == "juliaN")      = juliaN (read $ head pars) (read $ pars !! 1)
    | (fName == "affine")      = affine (AffineMatrix (read $ pars !! 0) (read $ pars !! 1) (read $ pars !! 2) (read $ pars !! 3) (read $ pars !! 4) (read $ pars !! 5))
    | (fName == "spherical")   = spherical
    | (fName == "linear")      = linear
    | (fName == "sinusoidal")  = sinusoidal
    | (fName == "swirl")       = swirl
    | (fName == "horseshoe")   = horseshoe
    | (fName == "polar")       = polar
    | (fName == "disc")        = disc
    | (fName == "spiral")      = spiral
    | (fName == "hyperbolic")  = hyperbolic
    | (fName == "square")      = square
    | (fName == "eyefish")     = eyefish
    | (fName == "bubble")      = bubble
    | (fName == "cylinder")    = cylinder
    | (fName == "noise")       = noise
    | (fName == "blur")        = blur
    | (fName == "gaussian")    = gaussian
    | (fName == "exponential") = exponential (read $ head pars) (read $ pars !! 1)
    | (fName == "eachSquare")  = eachSquare
    | (fName == "hyperb")      = hyperb
    | (fName == "sumMultAxis") = sumMultAxis
    | (fName == "mirrorX")     = mirrorX
    | (fName == "mirrorY")     = mirrorY
    | (fName == "mirrorR")     = mirrorR
    | otherwise                = (\ (GVec g _) -> GVec g (read fName,0)) 
    
delim :: [Char]
delim = ['+',':','*']

getOpAndRest :: String -> ( (Variation -> Variation -> Variation), String )
getOpAndRest [] = ((.), [])
getOpAndRest str  = (getop charOp, rest)
  where
     (charOp:rest) = dropWhile (== ' ') str
     getop '+' = (+)
     getop '*' = (*)
     getop _   = (.)


parseVar :: String -> Variation
parseVar [] = id
parseVar str | ( head str == ' ' ) = parseVar $ tail str
parseVar str | ( head str == '(' ) = op curV restV
   where (cur, restWithOp) = parseParen str
         curV        = parseVar cur
         restV      = parseVar rest
         (op, rest) = getOpAndRest restWithOp        

parseVar str | otherwise = op curV restV
   where cur   = takeWhile (\x -> not $ elem x delim) str
         curV  = getVar $ words cur
         restWithOp  = dropWhile (\x -> not $ elem x delim) str
         (op, rest) = getOpAndRest restWithOp
         restV = parseVar rest


-- | Paretheses parsing
parseParen :: String -> (String, String)
parseParen str = parseParen' 0 [] str
parseParen' :: Int -> String -> String -> (String,String)
parseParen' i pr (x:xs) | (x == '(' && i == 0) = parseParen' (i+1) pr xs
                        | (x == '(')           = parseParen' (i+1) (pr ++ [x]) xs
                        | (x == ')' && i == 1) = (pr, xs)
                        | (x == ')')           = parseParen' (i-1) (pr ++ [x]) xs
                        | otherwise            = parseParen' i (pr ++ [x]) xs