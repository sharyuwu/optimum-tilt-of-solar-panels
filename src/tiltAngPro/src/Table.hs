module Table where

    import Text.Layout.Table
    import Data.List
    import Calculation
    import DayADT
    import Data.Set (Set)
    import qualified Data.Set as Set

    addResult:: Set DegreeT -> Set Double -> Set DayT -> IO()
    addResult angle energy day = displayMainTable mainTable >> displayAngleTable day angle
        where mainTable = (Set.size angle, localAverage energy)

    localAverage:: Set Double -> Double
    localAverage energy = (/) (Set.foldr (+) 0 energy) (fromIntegral $ Set.size energy)

    displayMainTable:: (Int, Double) -> IO()
    displayMainTable (cutime, energy) = do
        content <- readFile "MainTable.txt"
        if null content
            then let mainTableContent = tableString [def , numCol]
                                        unicodeRoundS
                                        def
                                        [ rowG ["Adjust/time", show cutime]
                                        , rowG ["Energy Absorption/day", show energy]
                                        ] in
                writeFile "MainTable.txt" mainTableContent
            else let mainTableContent =  content ++ newContent
                        where newContent = tableString [ numCol]
                                           unicodeRoundS
                                           def
                                           [ rowG [show cutime ]
                                           , rowG [show energy ]
                                           ] in
                writeFile "MainTable.txt" mainTableContent

    displayAngleTable:: Set DayT -> Set DegreeT -> IO()
    displayAngleTable time angle = 
        appendFile "AngleTable.txt" content
            where content = tableString [fixedLeftCol 10, column (fixed 20) center dotAlign def]
                                        unicodeS
                                        (titlesH ["Time", "Optimum Angle"])
                                        $timeAndAngle (Set.toList time) (Set.toList angle)

    
    timeAndAngle:: [DayT] -> [DegreeT] -> [RowGroup]
    timeAndAngle  [x]    [y]    = [rowG [show x, show y]]
    timeAndAngle (x:xs) (y:ys)  = timeAndAngle [x] [y] ++ timeAndAngle xs ys

