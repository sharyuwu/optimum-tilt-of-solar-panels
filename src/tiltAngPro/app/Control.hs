module Control where

    import Lib
    import DayADT
    import Data.Time
    import TiltAngle
    import Energy
    import Calculation
    import Table

    import Data.String
    import Data.Set (Set)
    import qualified Data.Set as Set

    scaleList:: [DegreeT] -> Int -> [DegreeT]
    scaleList list diff
        | diff > length list = scaleList (list ++ take takeHowMany list) $diff-takeHowMany
        | otherwise              = list
            where takeHowMany
                    |diff - length list > length list = length list
                    |otherwise                        = diff - length list
    getOneAng:: [DegreeT] -> DayT -> DayT ->Double -> Double -> DegreeT -> IO()
    getOneAng zenList dayS dayE pw ph latitude =
        let theta_s_date = getzenList (scaleList zenList scale) dropPoint diff latitude
                                where dropPoint = fromInteger $perihelion dayS
                                      diff      = fromInteger $diffDays dayS dayE
                                      scale     = diff + dropPoint
        in --putStr . unlines $ map show theta_s_date
        let theta_T = getTilt theta_s_date baseCase
                            where baseCase = sumSunIn theta_s_date 1.35
        in --putStr $show theta_T
            --putStr $show $sumSunIn theta_s_date 1.35
        let p_e = getenergy pw ph localTiltSunIn theta_s_date
                        where localTiltSunIn = sglSunIn theta_T baseCase
                              baseCase = sumSunIn theta_s_date 1.35
        in --putStr . unlines $ map show $Set.toList p_e
        addResult (Set.singleton theta_T) p_e $Set.singleton dayS
        

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
        --let dec  = read inputAnalemma ::String
        let decList = lines inputAnalemma
        let decListToDegreeT = map read decList :: [DegreeT]
        let dayS = fromGregorian (read inputStartYear ::Integer) (read inputStartMonth ::Int) (read inputStartDay ::Int)
        let dayE = fromGregorian (read  inputEndYear ::Integer) (read inputEndtMonth ::Int) (read inputEndtDay ::Int)
        let pw = read inputPw ::Double
        let ph = read inputPh ::Double
        let latitude = read inputLatitude ::Double
        --putStr $unlines decList
        getOneAng decListToDegreeT dayS dayE pw ph latitude

        {- putStr latitude
        putStr contents
        putStr startDay
        putStr endDay
        putStr pw
        putStr ph-}

