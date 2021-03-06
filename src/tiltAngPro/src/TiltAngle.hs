module TiltAngle
    (getTilt
    )where
{-
    This module content the equation for calculation optimum
    tilt angle. This module corresponding to Optimum Tilt Angle 
    Module in MIS.
    getTilt is a global function.
-}    
import Calculation
import Data.List
import Data.Ord
import Data.Function
import SunCatTy
import Data.Maybe

-- |getTilt select the optimum tilt angle by getting the maximum
-- |value of the sun intensity corresponding to each angle.
-- |The angle has the maximum sun intensity is the optimum tilt angle.
--getTilt :: [DegreeT] -> Double -> DegreeT
--getTilt zenList sunInt = maximumBy (compare `on` sglSunIn sunInt) zenList

getTilt :: [DegreeT] -> DegreeT -> DegreeT
getTilt zenList tilt = maximumBy (compare `on` sumSunIn zenList) list
  where list = map (tilt + ) zenList
--getTilt zenList tilt = maximumBy (compare `on` sglSunIn tilt) zenList
{-getTilt zenList tilt
  | tilt == 0 = maximumBy (compare `on` sumSunIn zenList) zenList
  | otherwise = maximumBy (compare `on` sglSunIn tilt) zenList-}

--getTilt zenList tilt = maximumBy (compare `on` sglSunIn tilt) zenList
