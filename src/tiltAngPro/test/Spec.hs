-- test/Spec.hs
import Lib
import System.IO

import SunCatTy
import DayADT
import Data.Time
import InputVer
import Control


main :: IO ()
main = do
  inputHandle <- openFile "test/input.txt" ReadMode
  outputHandle <- openFile "test/terminal_output.txt" WriteMode
  runTiltAngPro inputHandle outputHandle
  hClose inputHandle
  hClose outputHandle

runTiltAngPro :: Handle -> Handle -> IO ()
runTiltAngPro inHandle outHandle = do
    latitude <- getLatitude inHandle outHandle
    startYear <- getStartYear inHandle outHandle
    startMonth <- getStartMonth inHandle outHandle
    startDay <- getStartDay inHandle outHandle
    endYear <- getEndYear inHandle outHandle
    endMonth <- getEndtMonth inHandle outHandle
    endDay <- getEndtDay inHandle outHandle
    pw <- getPw inHandle outHandle
    ph <- getPh inHandle outHandle
    input latitude startYear startMonth startDay endYear endMonth endDay pw ph



getLatitude, getStartYear, getStartMonth, getStartDay :: Handle -> Handle -> IO String
getEndYear, getEndtMonth, getEndtDay, getPw, getPh :: Handle -> Handle -> IO String

getLatitude inHandle outHandle = do
  hPutStrLn outHandle "Input the latitude"
  hGetLine inHandle

getStartYear inHandle outHandle = do
  hPutStrLn outHandle "Input the Start Year"
  hGetLine inHandle

getStartDay inHandle outHandle = do
  hPutStrLn outHandle "Input the Start Month"
  hGetLine inHandle 

getStartMonth inHandle outHandle = do
  hPutStrLn outHandle "Input the Start Day"
  hGetLine inHandle

getEndYear inHandle outHandle = do
  hPutStrLn outHandle "Input the End Year"
  hGetLine inHandle

getEndtMonth inHandle outHandle = do
  hPutStrLn outHandle "Input the End Month"
  hGetLine inHandle

getEndtDay inHandle outHandle = do
  hPutStrLn outHandle "Input the End Day"
  hGetLine inHandle

getPw inHandle outHandle = do
  hPutStrLn outHandle "Input the weight of your solar panel"
  hGetLine inHandle

getPh inHandle outHandle = do
  hPutStrLn outHandle "Input the height of your solar panel"
  hGetLine inHandle