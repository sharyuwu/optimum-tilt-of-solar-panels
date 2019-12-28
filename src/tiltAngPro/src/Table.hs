module Table where

import Text.Layout.Table
import Data.List
import Calculation
import DayADT
import SunCatTy

import Data.Set (Set)
import qualified Data.Set as Set

addResult :: [(Int, Int)] -> [DegreeT] -> [Double] -> [DayT] -> Int -> IO()
addResult cut angle energy day len = displayMainTable cut energy len >> displayAngleTable cut day angle


displayMainTable :: [(Int, Int)] -> [Double] -> Int -> IO()
displayMainTable list energy len = 
  writeFile "MainTable.txt" content
    where content = tableString [fixedLeftCol 15, column (fixed 20) center dotAlign def]
                                unicodeS
                                (titlesH ["Adjust/time", "Energy/day"])
                                $ map (\x -> timeAndAngergy x energy len) list

timeAndAngergy :: (Int, Int) -> [Double] -> Int -> RowGroup
timeAndAngergy (x, y) energy len = rowG [show y, show average]
    where
      list = take y $ drop x energy
      average = sum list / toEnum len 

displayAngleTable ::  [(Int, Int)] -> [DayT] -> [DegreeT] -> IO()
displayAngleTable list time angle = 
   writeFile "AngleTable.txt" content
      where content = tableString [fixedLeftCol 10, column (fixed 20) center dotAlign def]
                                unicodeS
                                (titlesH ["Time", "Optimum Angle"])
                                $ timeAndAngle time angle

    
timeAndAngle:: [DayT] -> [DegreeT] -> [RowGroup]
timeAndAngle [x] [y] = [rowG [show x, show y]]
timeAndAngle (x:xs) (y:ys)  = timeAndAngle [x] [y] ++ timeAndAngle xs ys

