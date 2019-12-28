module Energy
    ( getEnergy
    )where

{-
    This module content the equation for calculation solar energy
    absorption. This module corresponding to Solar Energy Absorption 
    Module in MIS. 
    getEnergy is a global function.
    localSunIn and localEnergy are local function used by getEnergy.
-}
import Calculation
import SunCatTy
import Data.Set (Set)
import qualified Data.Set as Set
{-
getEnergy get the weight and height of the solar panel, maxIten 
correspoding to the optimum tilt angle and the list of zenith angle,
and then return a set of energy absorption.
pw denotes the weight of the solar panel.
ph denotes the height of the solar panel.
maxInten denotes value of sun intensity corresponding to the optimum
tilt angle.
zenList denotes the list of zenith angle.
-}
getEnergy :: Double -> Double -> DegreeT -> [DegreeT] -> Double
getEnergy pw ph tilt zenList = sum $ localEnergy (localSunIn zenList tilt) pw ph
    --sum $ localEnergy (localSunIn zenList maxInten) pw ph

-- | localSunIn calculate the sun intensity corresponding to the 
-- | situation of solar panel is adjust to the optimum tilt angle.

localSunIn :: [DegreeT] -> DegreeT -> [Double]
localSunIn zenList tilt = map (`sglSunIn` tilt) zenList

-- | localEnergy calculate the energy absorption.
localEnergy :: [Double] -> Double -> Double -> [Double]
localEnergy sunIn pw ph = map ((pw * ph * 1.87 * 0.75) *) sunIn

