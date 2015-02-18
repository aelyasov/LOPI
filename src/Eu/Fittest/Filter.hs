module Eu.Fittest.Filter  where

--import Eu.Fittest.RPInferencer 
import Control.Monad (foldM)
import Eu.Fittest.Data
import Text.Regex.Posix
import Debug.Trace

type ConfLevelPred = (Int, Int) -> Bool

readCP :: String -> ConfLevelPred
readCP f
  | f == "defConfLevelPred"    = defConfLevelPred
  | f == "filterNegative"      = filterNegative
  | f == "filterNonNegative"   = filterNonNegative
  | f == "filterZero"          = filterZero
  | f == "filterBothZero"      = filterBothZero
  | f == "doesNotFilter"       = doesNotFilter
  | f =~ "0?\\.[0-9]*" :: Bool = userDefConfLevelPred (read f :: Float)
  | f =~ "[1-9][0-9]*" :: Bool = thresholdValue (read f :: Int)
  | otherwise                = error "undefined filter option"
 

type ConfLevel = Float

-- | This is confidence level used by default.
defConfLevel :: Float
defConfLevel = 0.99

-- | This function is conf.level function that used in computations by default.
-- | We can tune this function with the length of State data type
defConfLevelPred :: ConfLevelPred
defConfLevelPred (p,n) | n == 0    = (1 - (0.5)^p) > defConfLevel
                       | otherwise = False

userDefConfLevelPred :: ConfLevel -> ConfLevelPred
userDefConfLevelPred conf (p,n) | n == 0    = (1 - (0.5)^p) > conf
                                | otherwise = False

thresholdValue :: Int -> ConfLevelPred
thresholdValue i (p,n) | n == 0 && p >= i = True
                       | otherwise        = False
                                              
filterNegative :: ConfLevelPred
filterNegative (_,n) | n == 0    = True
                     | otherwise = False

filterNonNegative :: ConfLevelPred
filterNonNegative (_,n) | n > 0     = True
                        | otherwise = False

filterZero :: ConfLevelPred
filterZero (p,n) | p /= 0 && n == 0 = True
                 | otherwise        = False

filterBothZero :: ConfLevelPred
filterBothZero (p,n) | p == 0 && n == 0 = False
                     | otherwise        = True

doesNotFilter :: ConfLevelPred 
doesNotFilter (_,_) = True

data Filter = Filter ConfLevelPred

filterWitnesses :: Filter -> [Witness] -> [Witness]
filterWitnesses (Filter clPred) = filter (\(WitnessC _ _ (p,n)) -> clPred (p,n)) 
