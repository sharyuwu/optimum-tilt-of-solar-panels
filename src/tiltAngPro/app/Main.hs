module Main where

import Lib
import SunCatTy
import Data.Time
import Control

main :: IO ()
main = do
  putStr "Input the latitude"
  latitude <- getLine  
  putStr "Input the Start Year"
  startYear <- getLine
  putStr "Input the Start Month"
  startMonth <- getLine
  putStr "Input the Start Day"
  startDay <- getLine
  putStr "Input the End Year"
  endYear <- getLine
  putStr "Input the End Month"
  endMonth <- getLine
  putStr "Input the End Day"
  endDay <- getLine
  putStr "Input the weight of your solar panel"
  pw <- getLine
  putStr "Input the height of your solar panel"
  ph <- getLine
  input latitude startYear startMonth startDay endYear endMonth endDay pw ph