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

--getOneAng :: [DegreeT] -> DayT -> DayT ->Double -> Double -> DegreeT -> IO()
--getOneAng :: [DegreeT] -> DayT -> DayT ->Double -> Double -> DegreeT -> IO()
getPE :: [DegreeT] -> DegreeT ->Double -> Double -> Set Double
--getTwoAng :: [DegreeT] -> DayT -> DayT ->Double -> Double -> DegreeT -> IO()
{-getTwoAng theta_s_date dayS dayE pw ph latitude =
  let result1 = getOneAng xs dayS (addDay diff dayS) pw ph latitude
      result2 = getOneAng ys (addDay diff dayS) dayE pw ph latitude
        where 
          diff    = floor (abs . fromInteger $ diffDays dayE dayS) / 2
          xs = take diff  theta_s_date
          ys = drop diff  theta_s_date
      collect = (a `Set.union` a1, b `Set.union` b1, c `Set.union` c1)
        where (a,b,c) =  result1 
              (a1,b1,c1) =  result2
  in collect
    --(Set.singleton theta_T,p_e, Set.singleton dayS)-}

getAngle :: [DegreeT] ->  DegreeT
getThetaSDate :: [DegreeT] -> DayT -> DayT -> DegreeT -> [DegreeT]

getAngle theta_s_date =
      getTilt theta_s_date baseCase
        where 
          baseCase = sumSunIn theta_s_date 1.35
--putStr $show theta_T

getThetaSDate decList dayS dayE latitude =
  let a = getzenList (scaleList decList scale) dropPoint diff latitude
            where 
              dropPoint = abs . fromInteger $ perihelion dayS
              diff      = abs . fromInteger $ diffDays dayE dayS
              scale     = diff + dropPoint
      in a

getPE theta_s_date theta_T pw ph =
  let a = getEnergy pw ph localTiltSunIn theta_s_date
            where 
              localTiltSunIn = sglSunIn theta_T baseCase
              baseCase = sumSunIn theta_s_date 1.35
    in a
    --putStr $show theta_T
    --putStr $show $sumSunIn theta_s_date 1.35
    --(Set.singleton theta_T,p_e, Set.singleton dayS) 
    --putStr . unlines $ map show $Set.toList p_e
  --addResult (Set.singleton theta_T) p_e $Set.singleton dayS
        
scaleList:: [DegreeT] -> Int -> [DegreeT]
scaleList list diff
    | diff > len  = scaleList (list ++ take haveTakeDay list) $ diff-haveTakeDay
    | otherwise   = list
        where 
          len = length list
          diffLen = diff - len
          haveTakeDay
            |diffLen > len = len
            |otherwise     = diffLen
                        

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
    writeFile "MainTable.txt" ""
    writeFile "AngleTable.txt" ""
    if verifiedP pw1 ph1 && verifiedD dayS dayE && verifiedLat latitude1
    then 
      let thetha_s_data = getThetaSDate decList dayS dayE latitude1
          tiltAngle = getAngle thetha_s_data
          energy = getPE thetha_s_data tiltAngle pw1 ph1
      in addResult (Set.singleton tiltAngle) energy (Set.singleton dayS)
     {- let theta_s_date = getzenList (scaleList decList scale) dropPoint diff latitude
            where 
              dropPoint = abs . fromInteger $ perihelion dayS
              diff      = abs . fromInteger $ diffDays dayE dayS
              scale     = diff + dropPoint
          --putStr . unlines $ map show theta_s_date
          collect = getOneAng theta_s_date dayS dayE pw1 ph1 latitude1
        in addResult collect-}
    else putStr "Incorrect input data"


