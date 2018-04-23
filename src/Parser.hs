{-# OPTIONS_GHC -w #-}
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

-- contents <- readFile "a.txt"
-- words $ contents

-- | read a model from the file
parseModel :: [String] -> Model -> Model  
parseModel (fName : fVal : rest) mod 
    | (fName == "mName")             = parseModel rest mod {mName = fVal}
    | (fName == "mTransforms")       = let 
                                       l = (splitOn ["endT"] (fVal : rest))
                                       trans = map (parseTran templateTransform) (init l)
                                       r = last l 
    								   in parseModel r mod {mTransforms = trans}
    | (fName == "mFinal")            = let (t:r:[]) = (splitOn ["endF"] (fVal : rest)) 
									   in parseModel r mod {mFinal = Just (parseTran templateTransform t )}
    | (fName == "mGradient")         = parseModel rest mod {mGradient = paletteToDouble fVal}
    | (fName == "mWidth")            = parseModel rest mod {mWidth = read fVal :: Int}
    | (fName == "mHeight")           = parseModel rest mod {mHeight = read fVal :: Int}
    | (fName == "mScale")            = parseModel rest mod {mScale = read fVal :: Double}
    | (fName == "mShiftX")           = parseModel rest mod {mShiftX = read fVal :: Double}
    | (fName == "mShiftY")           = parseModel rest mod {mShiftY = read fVal :: Double}
    | (fName == "mRotation")         = parseModel rest mod {mRotation = read fVal :: Double}
    | (fName == "mBackgroundColour") = parseModel (drop 2 rest) mod {mBackgroundColour = (\_ -> (read fVal :: Double, read (head rest) :: Double, read (head $ tail $ rest) :: Double, 1))}
    | (fName == "mOuterIter")        = parseModel rest mod {mOuterIter = read fVal :: Int}
    | (fName == "mInnerIter")        = parseModel rest mod {mInnerIter = read fVal :: Int}
    | otherwise                      = mod

-- | parse a single transform
parseTran :: Transform -> [String] -> Transform
parseTran tran (fName : fVal : rest)
  --  (fName == "tVariation") =
  	| (fName == "tWeight") = parseTran tran {tWeight = read fVal :: Double} rest
  	| (fName == "tColorPosition") = parseTran tran {tColorPosition = read fVal :: Double} rest
  	| (fName == "tColorSpeed") = parseTran tran {tColorSpeed = read fVal :: Double} rest
  	| (fName == "tOpacity") = parseTran tran {tOpacity = read fVal :: Double} rest
  	--  (fName == "tXaos") = 

--parseVar :: String -> Variation

-- S = F [OP F]
--  F = variation [params] | (S)
-- OP = . | + | *

-- blur . ( id + juliaN 12 3)