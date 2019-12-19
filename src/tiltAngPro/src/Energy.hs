module Energy where

    import Calculation
    import Data.Set (Set)
    import qualified Data.Set as Set

    getenergy:: Double -> Double -> Double -> [DegreeT] -> Set Double
    getenergy pw ph maxInten zenList = localEnergy (localSunIn zenList maxInten) pw ph

    localSunIn:: [DegreeT] -> Double -> Set Double
    localSunIn zenList maxInten = Set.fromList $map (sglSunIn maxInten) zenList


    localEnergy:: Set Double -> Double -> Double -> Set Double
    localEnergy sunIn pw ph = Set.map ((pw * ph * 1.87 * 0.75) *) sunIn