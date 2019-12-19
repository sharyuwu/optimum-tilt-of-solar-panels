module Calculation where

    import Data.List
    type DegreeT = Double


    getzenList:: [DegreeT] -> Int -> Int -> Double -> [DegreeT]
    getzenList decList i diff latitude = map (zenAngle latitude) $drop i decList ++ take diff decList
    --getzenList decList i diff latitude = map (zenAngle latitude) $drop i decList ++ take diff decList 

    zenAngle:: DegreeT -> Double -> DegreeT
    zenAngle dec lat
        | lat * dec < 0  = dec + lat
        | otherwise      = dec - lat
    
    sunInten:: Double
    sunInten = 1.35

    sumSunIn:: [DegreeT] -> Double -> Double
    sumSunIn zenList energy = foldr ((+) . sglSunIn energy) 0 zenList

    sglSunIn:: DegreeT -> Double -> Double
    sglSunIn zen energy = sunInten * (1.0 / energy) ** (1.0/cos zen * 0.0174533)
    --sglSunIn zen energy = sunInten * (1.0 / energy) ** (1.0/cos zen * 0.0174533)



