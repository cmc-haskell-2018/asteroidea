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

splitt :: Char -> String -> [String]
splitt _ "" = []
splitt c s = firstWord : (splitt c rest)
 where firstWord = takeWhile (/=c) s
       rest = drop (length firstWord + 1) s

-- contents <- readFile "a.txt"
-- words $ contents

parseModel :: [String] -> Model -> Model  
parseModel (fName : fVal : rest) mod 
    | (fName == "mName")             = (parseModel rest mod {mName = fVal})
 -- | (fName == "mTransforms")       = 
 -- | (fName == "mFinal")            =
 	| (fName == "mGradient")         = (parseModel rest mod {mGradient = paletteToDouble fVal})
 	| (fName == "mWidth")            = (parseModel rest mod {mWidth = read fVal :: Int})
 	| (fName == "mHeight")           = (parseModel rest mod {mHeight = read fVal :: Int})
 	| (fName == "mScale")            = (parseModel rest mod {mScale = read fVal :: Double})
 	| (fName == "mShiftX")           = (parseModel rest mod {mShiftX = read fVal :: Double})
 	| (fName == "mShiftY")           = (parseModel rest mod {mShiftY = read fVal :: Double})
 	| (fName == "mRotation")         = (parseModel rest mod {mRotation = read fVal :: Double})
 	| (fName == "mBackgroundColour") = (parseModel (tail $ tail $ rest) mod {mBackgroundColour = (\_ -> (read fVal :: Double, read (head rest) :: Double, read (head $ tail $ rest) :: Double, 1))})
 	| (fName == "mOuterIter")        = (parseModel rest mod {mOuterIter = read fVal :: Int})
 	| (fName == "mInnerIter")        = (parseModel rest mod {mInnerIter = read fVal :: Int})

--parseTrans :: String -> [Transform]

--parseVar :: String -> Variation

-- S = F [OP F]
--  F = variation [params] | (S)
-- OP = . | + | *

-- blur . ( id + juliaN 12 3)