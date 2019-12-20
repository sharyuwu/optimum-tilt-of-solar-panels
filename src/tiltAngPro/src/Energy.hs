module Energy
    ( getenergy
    )where

{-
    This module content the equation for calculation solar energy
    absorption. This module corresponding to Solar Energy Absorption 
    Module in MIS. 
    getenergy is a global function.
    localSunIn and localEnergy are local function used by getenergy.
-}
    import Calculation
    import Data.Set (Set)
    import qualified Data.Set as Set
{-
getenergy get the weight and height of the solar panel, maxIten 
correspoding to the optimum tilt angle and the list of zenith angle,
and then return a set of energy absorption.
pw denotes the weight of the solar panel.
ph denotes the height of the solar panel.
maxInten denotes value of sun intensity corresponding to the optimum
tilt angle.
zenList denotes the list of zenith angle.
-}
    getenergy :: Double -> Double -> Double -> [DegreeT] -> Set Double
    getenergy pw ph maxInten zenList = localEnergy (localSunIn zenList maxInten) pw ph

-- | localSunIn calculate the sun intensity corresponding to the 
-- | situation of solar panel is adjust to the optimum tilt angle.

    localSunIn :: [DegreeT] -> Double -> Set Double
    localSunIn zenList maxInten = Set.fromList $ map (sglSunIn maxInten) zenList

-- | localEnergy calculate the energy absorption.
    localEnergy :: Set Double -> Double -> Double -> Set Double
    localEnergy sunIn pw ph = Set.map ((pw * ph * 1.87 * 0.75) *) sunIn