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
parseModel :: [String] -> Model  
parseModel ("mName" : nam : "mTransforms" : trans : "mFinal" : fin : "mGradient" : grad : "mWidth" : width : "mHeight" : height : "mScale" : scale : 
	       "mShiftX" : shX : "mShiftY" : shY : "mRotation" : rot : "mBackgroundColour" : b1 : b2 : b3 : "mOuterIter" : out : "mInnerIter" : inner : _) =
	templateModel { mName = nam
     			--, mTransforms = parseTrans trans
     			--, mFinal = head $ parseTrans $ final
     			  , mGradient = paletteToDouble grad
     			  , mWidth = read width :: Int
     			  , mHeight = read height :: Int
     			  , mScale = read scale :: Double
     			  , mShiftX = read shX :: Double
     			  , mShiftY = read shY :: Double 
     			  , mRotation = read rot :: Double
     			  , mBackgroundColour = (\_ -> (read b1 :: Double,read b2 :: Double, read b3 :: Double, 1))
     			  , mOuterIter = read out :: Int
     			  , mInnerIter = read inner :: Int
     			  }


--parseTrans :: String -> [Transform]

--parseVar :: String -> Variation

-- S = F [OP F]
--  F = variation [params] | (S)
-- OP = . | + | *

-- blur . ( id + juliaN 12 3)