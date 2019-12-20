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
import SunCatTy
import Data.Maybe

-- |getTilt select the optimum tilt angle by getting the maximum
-- |value of the sun intensity corresponding to each angle.
-- |The angle has the maximum sun intensity is the optimum tilt angle.
getTilt :: [DegreeT] -> Double -> DegreeT
getTilt zenList sunInt = 
    let list = map (sglSunIn sunInt) zenList in
    let max = maximum list in 
    let index = fromJust $elemIndex max list in
         last $ take index zenList

