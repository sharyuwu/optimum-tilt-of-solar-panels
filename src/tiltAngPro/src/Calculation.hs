
module Calculation 
    (getzenList, 
    sumSunIn,
    sglSunIn,
    )where


{-
    This module content the equation for calculation single 
    Sun Intensity, the sum of the sun intensity and the zenith 
    angle. This module corresponding to the Calculation module
    designed in MIS for this program.
-}
import Data.List
import SunCatTy
{-
getzenList get the list of sun declination angle and then
output a list of zenith angle.
The lengh of the list is corresponding to the start day and
the end day. i denode the start day. diff denode how mant days
is between start day and end day.
-}
getzenList :: [DegreeT] -> Int -> Int -> Double -> [DegreeT]
getzenList decList i diff latitude = map (zenAngle latitude) . take diff $ drop i decList
-- take diff $ drop i decList
--drop i decList ++ take diff decList
{-
zenAngle get the sun declination and the latitude, and then
calculate the zenith angle.
-}
zenAngle :: DegreeT -> Double -> DegreeT
zenAngle dec lat
    | lat * dec < 0  = dec + lat
    | otherwise      = dec - lat
    
-- |sunInten denotes the base case of the sun intensity     
sunInten :: Double
sunInten = 1.35

-- |sumSunIn denotes summation of the single sun intensity     
sumSunIn :: [DegreeT] -> Double -> Double
sumSunIn zenList energy = sum $ map (sglSunIn energy) zenList
        -- foldr ((+) . sglSunIn energy) 0 zenList
    

-- |sglSunIn denotes the single sun intensity 
sglSunIn :: DegreeT -> Double -> Double
sglSunIn zen energy = sunInten * (1.0 / energy) ** (1.0/cos (zen * 0.0174533))



