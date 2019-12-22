-- test/Spec.hs
import Lib
import System.IO

import SunCatTy
import DayADT
import InputVer
import Control
import Calculation

import qualified Data.ByteString.Lazy.Char8 as BS
import System.FilePath
import System.FilePath.Glob
import Data.List
import Data.Time
import Data.Maybe
import Data.String
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden


{-
  Default Values : 
  sun declination angle = 20 in id4.calculation
  sun intensity = 1.35 in id5.calculation
-}

deltaDate :: [Double]
deltaDate = [20]

sunInten :: Double
sunInten = 1.35

main :: IO ()
main = do
  inputHandle <- openFile "test/tests/input.txt" ReadMode
  outputHandle <- openFile "test/tests/terminal_output.txt" WriteMode
  runTiltAngPro inputHandle outputHandle
  hClose inputHandle
  hClose outputHandle
  paths <- testFilesForInputReading
  goldens <- mapM mkGoldenTest paths
  pathCal <- testFilesForCalculation
  goldenCal <- mapM mkGoldenTest pathCal
  let testG = goldens ++ goldenCal
  defaultMain (testGroup "Test" $ testG ++ [testsUnit])

  
testsUnit :: TestTree
testsUnit = testGroup "Unit tests" [inputBoundsTests_id1, inputBoundsTests_id2, inputBoundsTests_id3]

testFilesForInputReading :: IO [FilePath]
testFilesForInputReading = globDir1 pat "test/tests"
  where pat = compile "*.inputReading"

testFilesForCalculation :: IO [FilePath]
testFilesForCalculation = globDir1 pat "test/tests"
  where pat = compile "*.calculation"


mkGoldenTest :: FilePath -> IO TestTree
mkGoldenTest path = 
  let 
      goldenPath = replaceExtension path $ testE ++ ".golden"
      in return (goldenVsString testN goldenPath action)
          where
            testN = takeBaseName path
            testE = takeExtension path
            action :: IO BS.ByteString
            action = do
              script <- readFile path
              let actual = testE ++ "\n" ++ test testN testE script
                in return (BS.pack actual)

test :: String -> String -> String -> String
test "id1" ".inputReading" s =
  let linesS = lines s in
    case linesS of
      (x:xs) -> test_id1_IR x xs
      []     -> "This is an empty string."

test "id2" ".inputReading" s =
  let linesS = lines s in
    case linesS of
        (x:xs) -> test_id2_IR x xs
        []     -> "This is an empty string."
test "id3" ".inputReading" s =
  let linesS = lines s in
    case linesS of
        (x:xs) -> test_id3_IR x xs
        []     -> "This is an empty string."

test "id4" ".calculation" s =
  let linesS = lines s in
    case linesS of
        (x:xs) -> test_id4_Cal x xs
        []     -> "This is an empty string."

test "id5" ".calculation" s =
  let linesS = lines s in
    case linesS of
        (x:xs) -> test_id5_Cal x xs
        []     -> "This is an empty string."
        

test_id1_IR, test_id2_IR, test_id3_IR :: String -> [String] -> String
test_id4_Cal, test_id5_Cal :: String -> [String] -> String
test_id1_IR "id1_1" (x:y:ys) = 
  let actual = map read [x,y] :: [Double]
      expect = [1455, 665] :: [Double]
      result = "id1_1" : errorInputRead actual expect
      in unlines result ++ test_id1_IR (head ys) (tail ys)
test_id1_IR "id1_2" [x,y] = 
  let actual = map read [x,y] :: [Double]
      expect = [1455.54, 665.13] :: [Double]
      result = "id1_2" : errorInputRead actual expect
    in unlines result

test_id2_IR "id2_1" (x:xs) = 
  let actual = [read x] :: [Double]
      expect = [90] :: [Double]
      result = "id2_1" : errorInputRead actual expect
    in unlines result ++ test_id2_IR (head xs) (tail xs)

test_id2_IR "id2_2" (x:xs) = 
  let actual = [read x] :: [Double]
      expect = [-90] :: [Double]
      result = "id2_2" : errorInputRead actual expect
    in unlines result ++ test_id2_IR (head xs) (tail xs)

test_id2_IR "id2_3" (x:xs) = 
  let actual = [read x] :: [Double]
      expect = [3.2] :: [Double]
      result = "id2_3" : errorInputRead actual expect
      in unlines result ++ test_id2_IR (head xs) (tail xs)

test_id2_IR "id2_4" (x:xs) = 
  let actual = [read x] :: [Double]
      expect = [-3.2] :: [Double]
      result = "id2_4" : errorInputRead actual expect
      in unlines result ++ test_id2_IR (head xs) (tail xs)

test_id2_IR "id2_5" [x] = 
  let actual = [read x] :: [Double]
      expect = [0] :: [Double]
      result = "id2_5" : errorInputRead actual expect
    in unlines result

test_id3_IR "id3_1" (x:xs) = 
  let linesS = map read (x : take 5 xs) :: [Int]
      actual = makeDayList linesS
      expect = [fromGregorian 2020 02 28, fromGregorian 2021 02 28] :: [DayADT.DayT]
      result = "id3_1" : errorInputReadD actual expect
      in unlines result ++ test_id3_IR (head list) (tail list)
            where list = drop 5 xs

test_id3_IR "id3_2" (x:xs) = 
  let linesS = map read (x:xs) :: [Int]
      actual = makeDayList linesS
      expect = [fromGregorian 1996 01 03, fromGregorian 2000 01 14] :: [DayADT.DayT]
      result = "id3_2" : errorInputReadD actual expect
      in unlines result


test_id4_Cal "id4_1" (x:xs) =
  let actual = read x :: Double
      expect = [23.250943] :: [Double]
      calculation = getzenList deltaDate 0 1 actual
      result = "id4_1" : errorCal_id4 calculation expect
      in unlines result ++ test_id4_Cal (head xs) (tail xs)

test_id4_Cal "id4_2" (x:xs) =
  let actual = read x :: Double
      expect = [-23.250943] :: [Double]
      calculation = getzenList deltaDate 0 1 actual
      result = "id4_2" : errorCal_id4 calculation expect
      in unlines result ++ test_id4_Cal (head xs) (tail xs)

test_id4_Cal "id4_3" (x:xs) =
  let actual = read x :: Double
      expect = [-69.250943] :: [Double]
      calculation = getzenList deltaDate 0 1 actual
      result = "id4_3" : errorCal_id4 calculation expect
      in unlines result ++ test_id4_Cal (head xs) (tail xs)

test_id4_Cal "id4_4" [x] =
  let actual = read x :: Double
      expect = [69.250943] :: [Double]
      calculation = getzenList deltaDate 0 1 actual
      result = "id4_4" : errorCal_id4 calculation expect
      in unlines result

test_id5_Cal "id5_1" (x:xs) =
  let actual = read x :: Double
      expect = [0.9738212] :: [Double]
      calculation = sglSunIn actual sunInten
      result = "id5_1" : errorCal_id5 [calculation] expect
      in unlines result ++ test_id5_Cal (head xs) (tail xs)


test_id5_Cal "id5_2" (x:xs) =
  let actual = read x :: Double
      expect = [0.9738212] :: [Double]
      calculation = sglSunIn actual sunInten
      result = "id5_2" : errorCal_id5 [calculation] expect
      in unlines result ++ test_id5_Cal (head xs) (tail xs)

test_id5_Cal "id5_3" (x:xs) =
  let actual = read x :: Double
      expect = [0.5786897] :: [Double]
      calculation = sglSunIn actual sunInten
      result = "id5_3" : errorCal_id5 [calculation] expect
      in unlines result ++ test_id5_Cal (head xs) (tail xs)

test_id5_Cal "id5_4" (x:xs) =
  let actual = read x :: Double
      expect = [0.5786897] :: [Double]
      calculation = sglSunIn actual sunInten
      result = "id5_4" : errorCal_id5 [calculation] expect
      in unlines result


inputBoundsTests_id1 = testGroup "InputBounds tests id1"
  [ 
    testCase "id1_1" $
    verifiedLat 90 @?= True
  
    , testCase "id1_2" $
    verifiedLat (negate 90) @?= True

    , testCase "id1_3" $
    verifiedLat 89.9 @?= True

    , testCase "id1_4" $
    verifiedLat (negate 89.9) @?= True

    , testCase "id1_5" $
    verifiedLat 0 @?= True

    , testCase "id1_6" $
    verifiedLat 91 @?= False

    , testCase "id1_7" $
    verifiedLat (negate 91) @?= False

    , testCase "id1_8" $
    verifiedLat 90.1 @?= False

    , testCase "id1_9" $
    verifiedLat (negate 90.1) @?= False
  ]

inputBoundsTests_id2 = testGroup "InputBounds tests id2"
  [
    testCase "id2_1" $
    fromGregorianValid 0 0 0 @?= Nothing
  
    , testCase "id2_2" $
    fromGregorianValid (negate 1) (negate 1) (negate 1) @?= Nothing

    , testCase "id2_3" $
    fromGregorianValid 2020 (negate 1) 29 @?= Nothing

    , testCase "id2_4" $
    fromGregorianValid 2020 20 (negate 1) @?= Nothing

    , testCase "id2_5" $
    fromGregorianValid (negate 1) 02 29 @?= Nothing

    , testCase "id2_6" $
    fromGregorianValid 2020 13 (negate 1) @?= Nothing

    , testCase "id2_7" $
    fromJust (fromGregorianValid 2020 02 29) @?= fromGregorian 2020 02 29

    , testCase "id2_8" $
    fromJust (fromGregorianValid 2020 02 28) @?= fromGregorian 2020 02 28
  ]

inputBoundsTests_id3 = testGroup "InputBounds tests id3"
  [
    testCase "id3_1" $
    verifiedD (fromGregorian 2020 02 28) (fromGregorian 2021 02 28) @?= True
  
    , testCase "id3_2" $
    verifiedD (fromGregorian 2020 02 28) (fromGregorian 2019 02 28) @?= False

    , testCase "id3_3" $
    verifiedD (fromGregorian 2020 02 28) (fromGregorian 2020 01 28) @?= False

    , testCase "id3_4" $
    verifiedD (fromGregorian 2020 02 28) (fromGregorian 2020 02 27) @?= False

    , testCase "id3_5" $
    verifiedD (fromGregorian 2020 02 28) (fromGregorian 2020 02 28) @?= False
  ]




makeDayList :: [Int] -> [DayADT.DayT]
makeDayList [x, y, z] = [fromGregorian (toInteger x) y z]
makeDayList (x:y:z:zs) = makeDayList [x,y,z] ++ makeDayList zs

errorInputRead,errorCal_id4 :: [Double] -> [Double] -> [String] 
errorInputRead [x] [y] = [ "Input " ++ show x ++ " Absolute Erros = " ++ show (x - y) ]
errorInputRead (x:xs) (y:ys) = errorInputRead [x] [y] ++ errorInputRead xs ys

errorInputReadD :: [DayADT.DayT] -> [DayADT.DayT] -> [String] 
errorInputReadD [x] [y] = [ "Input " ++ showGregorian  x ++ " Absolute Erros = " ++ show (diffDays x y) ]
errorInputReadD (x:xs) (y:ys) = errorInputReadD [x] [y] ++ errorInputReadD xs ys

errorCal_id4 [x] [y] = [ "Input " ++ show x ++ " Output " ++ show y ++" Absolute Erros = " ++ show (x - y) ]
errorCal_id4 (x:xs) (y:ys) = errorCal_id4 [x] [y] ++ errorCal_id4 xs ys

errorCal_id5 :: [DegreeT] -> [Double] -> [String] 
errorCal_id5 [x] [y] = [ "Input " ++ show x ++ " Output " ++ show y ++ " Relative Erros = " ++ show (abs (1 -(x / y))) ]
errorCal_id5 (x:xs) (y:ys) = errorCal_id5 [x] [y] ++ errorCal_id5 xs ys

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