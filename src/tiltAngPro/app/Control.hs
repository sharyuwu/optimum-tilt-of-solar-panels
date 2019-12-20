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
getOneAng:: [DegreeT] -> DayT -> DayT ->Double -> Double -> DegreeT -> IO()
getOneAng decList dayS dayE pw ph latitude =
  let theta_s_date = getzenList (scaleList decList scale) dropPoint diff latitude
        where 
          dropPoint = abs . fromInteger $perihelion dayS
          diff      = abs . fromInteger $diffDays dayE dayS
          scale     = diff + dropPoint
      --putStr . unlines $ map show theta_s_date
      theta_T = getTilt theta_s_date baseCase
        where 
          baseCase = sumSunIn theta_s_date 1.35
    --putStr $show theta_T
            --putStr $show $sumSunIn theta_s_date 1.35
      p_e = getEnergy pw ph localTiltSunIn theta_s_date
        where 
          localTiltSunIn = sglSunIn theta_T baseCase
          baseCase = sumSunIn theta_s_date 1.35
  in --putStr . unlines $ map show $Set.toList p_e
  addResult (Set.singleton theta_T) p_e $Set.singleton dayS
        
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
                        
main :: IO ()
main = do
    inputAnalemma <- readFile "analemma.txt"
    putStr "Input the latitude"
    inputLatitude <- getLine  
    putStr "Input the Start Year"
    inputStartYear <- getLine
    putStr "Input the Start Month"
    inputStartMonth <- getLine
    putStr "Input the Start Day"
    inputStartDay <- getLine
    putStr "Input the End Year"
    inputEndYear <- getLine
    putStr "Input the End Month"
    inputEndtMonth <- getLine
    putStr "Input the End Day"
    inputEndtDay <- getLine
    putStr "Input the weight of your solar panel"
    inputPw <- getLine
    putStr "Input the height of your solar panel"
    inputPh <- getLine
    let decList = lines inputAnalemma
    let decListToDegreeT = map read decList :: [DegreeT]
    let dayS = fromGregorian (read inputStartYear ::Integer) (read inputStartMonth ::Int) (read inputStartDay ::Int)
    let dayE = fromGregorian (read  inputEndYear ::Integer) (read inputEndtMonth ::Int) (read inputEndtDay ::Int)
    let pw = read inputPw ::Double
    let ph = read inputPh ::Double
    let latitude = read inputLatitude ::Double
    writeFile "MainTable.txt" ""
    writeFile "AngleTable.txt" ""
    getOneAng decListToDegreeT dayS dayE pw ph latitude



