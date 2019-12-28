module Control where

{-
    This module content the process of getting input value 
    from users, and then out put one optimum angle or multiple
    optimum angle. This module corresponding to the Control Module
    designed in MIS for this program.
-}
import DayADT
import Data.Time
import TiltAngle
import Energy
import Calculation
import Table
import SunCatTy
import InputVer

import Data.List
import Data.String
import Data.Set (Set)
import qualified Data.Set as Set

{-
    getOneAng get a list of sun declination angles, the day of
    start day and end day, the weight and height of the solar 
    panel, and the valur of the latitude, and then it produce
    the result of one optimum angle and its corresponding energy
    absorption.
-}


baseSunInten :: [DegreeT] -> Double
baseSunInten list = sumSunIn list 1.35

cutTime :: Int
cutTime = 2

optAngle :: (Int, Integer) -> [DegreeT] ->  DegreeT -> DegreeT
optAngle (x, y) theta_s_date tiltBefore 
    | tiltBefore == 0                            =  optAngle (x, y) theta_s_date tiltAfter
    | intensity tiltBefore > intensity tiltAfter = tiltBefore
    | otherwise                                  = optAngle (x, y) theta_s_date tiltAfter
        where 
          intensity = sumSunIn list
          list = take (fromInteger y) $ drop (x * fromInteger y) theta_s_date
          tiltAfter = getTilt list tiltBefore
  --getTilt list (baseSunInten list)
 {-   let list = take (fromInteger y) $ drop (x * fromInteger y) theta_s_date
        tiltAfter = getTilt list tiltBefore
    in 
      if ((sum $ map (sumSunIn tiltBefore) list) > (sum $ map (sumSunIn tiltAfter) list))
      then tiltBefore
      else optAngle (x, y) theta_s_date tiltAfter -}
--putStr $show theta_T

optEnergy :: ((Int, Integer), Double) -> [DegreeT] -> Double -> Double -> Double
optEnergy ((x, y), ang) theta_s_date pw ph =
  getEnergy pw ph ang list
    where 
      --localTiltSunIn = sglSunIn ang (baseSunInten list)
      list = take (fromInteger y) $ drop (x * fromInteger y) theta_s_date
    --putStr $show theta_T
    --putStr $show $sumSunIn theta_s_date 1.35
    --(Set.singleton theta_T,p_e, Set.singleton dayS) 
    --putStr . unlines $ map show $Set.toList p_e
  --addResult (Set.singleton theta_T) p_e $Set.singleton dayS

zenithL :: [DegreeT] -> DayT -> DayT -> LatitudeT -> [DegreeT]
zenithL decList dayS dayE latitude =
  let scaleL = scaleList decList scale
        where scale = diff + dropPoint
      dropPoint = abs . fromInteger $ perihelion dayS
      diff      = abs . fromInteger $ diffDays dayE dayS
  in getzenList scaleL dropPoint diff latitude


scaleList :: [DegreeT] -> Int -> [DegreeT]
scaleList list diff
    | diff > len  = scaleList (list ++ take haveTakeDay list) $ diff-haveTakeDay
    | otherwise   = list
        where 
          len = length list
          diffLen = diff - len
          haveTakeDay
            |diffLen > len = len
            |otherwise     = diffLen
                        

makeCut :: Int -> Double -> [(Int, Integer)]
makeCut cut len =  netFst cut  (len / toEnum cut)
    where 
      netFst c le
        | c /= 0              = netSnd (c - 1) le
        | c == 0 && le /= len = makeCut (cut - 1) len
        | otherwise           = [ ]
        where netSnd c le = (c, round le) : netFst c le

intermidiate :: [DegreeT] -> DayT -> DayT -> Double -> Double -> LatitudeT -> IO ()
intermidiate dec dayS dayE pw ph latitude =
  let zenith = zenithL dec dayS dayE latitude
      cutList = makeCut cutTime $ toEnum (length zenith)
      angle = map (\x -> optAngle x zenith 0) cutList
      energy = map (\x -> optEnergy x zenith pw ph) zipCA
        where zipCA = zip cutList angle
      day = map (`getDayList` dayS) cutList
    in addResult (lisForResult cutTime) angle energy day (length zenith)
      --putStr . unlines $ map show cutList
      --putStr . unlines $ map show energy
      --addResult (lisForResult cutTime) angle energy day (length zenith)

lisForResult :: Int -> [(Int, Int)]
lisForResult cut = netFirst (cut-1) 0
  where netFirst c record
            | c < 0 = [ ]
            -- | c == 1 = [(c + 1 + record, 1)]
            | record == 0 = (0, c + 1) : netFirst (c-1) (c + 1)
            | otherwise = (record, c + 1) : netFirst (c-1) (c + 1 + record)

input :: String -> String -> String -> String -> String -> String-> String-> String-> String ->IO ()
input latitude startYear startMonth startDay endYear endMonth endDay pw ph = do
    inputAnalemma <- readFile "analemma.txt"
    let contentOfFile = lines inputAnalemma
    let decList = map read contentOfFile :: [DegreeT]
    let dayS = fromGregorian (read startYear :: Integer) (read startMonth :: Int) (read startDay :: Int)
    let dayE = fromGregorian (read  endYear :: Integer) (read endMonth :: Int) (read endDay :: Int)
    let pw1 = read pw :: Double
    let ph1 = read ph :: Double
    let latitude1 = read latitude :: Double
    if verifiedP pw1 ph1 && verifiedD dayS dayE && verifiedLat latitude1
     then intermidiate decList dayS dayE pw1 ph1 latitude1
     {- let theta_s_date = getzenList (scaleList decList scale) dropPoint diff latitude
            where 
              dropPoint = abs . fromInteger $ perihelion dayS
              diff      = abs . fromInteger $ diffDays dayE dayS
              scale     = diff + dropPoint
          --putStr . unlines $ map show theta_s_date
          collect = getOneAng theta_s_date dayS dayE pw1 ph1 latitude1
        in addResult collect-}
     else putStr "Incorrect input data"


