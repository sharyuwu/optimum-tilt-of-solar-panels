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
getTilt :: [DegreeT] -> Double -> DegreeT
getTilt zenList sunInt = maximumBy (compare `on` sglSunIn sunInt) zenList
    --snd $ maximumBy (compare `on` fst) $ map (\z -> (sglSunIn sunInt z, z)) zenList
    {- maximumBy (\(a,_) (b,_) -> compare (sglSunIn a) (sglSunIn b)) $ map (\z -> (sglSunIn z, z)) zenList
    = maximumBy (compare `on` sglSunIn sunInt) zenList
    --in snd element
    --let list = map (sglSunIn sunInt) zenList in
    --let max = maximum list in 
    --let index = fromJust $elemIndex max list in
    -- last $ take index zenList 
    -}