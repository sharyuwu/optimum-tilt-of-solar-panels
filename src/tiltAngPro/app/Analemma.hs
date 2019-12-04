module Analemma where

    import Data.Time
    
    x0:: Integer ->Float
    x0 d = let fd = fromInteger (negate d) / 365.25 * 360 * pi/180 in
        negate 0.9833 * sin fd
    
    y0:: Integer ->Float
    y0 d = let fd = fromInteger (negate d) / 365.25 * 360 * pi/180 in
        negate 0.9833 * cos fd
    
    y0':: Integer ->Float
    y0' d = y0 d * cos 23.44 * pi/180
    
    
    z0:: Integer ->Float
    z0 d = negate $ y0 d * sin 23.44 * pi/180
    
    baseDate:: Day
    baseDate = fromGregorian 2019 12 21
    
    endDate:: Day -- :: (year,month,day)
    endDate = addDays 366 baseDate
    
    nextDate:: Day -> Day -- :: (year,month,day)
    nextDate = addDays 1
    
    re:: Integer ->(Float, Float, Float)
    re a = (x0 a, y0' a, z0 a)
    
    relist:: Day -> [(Float, Float, Float)] -> [(Float, Float, Float)]
    relist d x
       | d == endDate = re 366 : x
       | otherwise = re (diffDays d baseDate) : relist (nextDate d) x
    
    showdata:: [(Float, Float, Float)]
    showdata = relist (addDays 2 baseDate) [re 1]
    
    inputFile:: (Float, Float, Float) -> IO()
    inputFile get = let (x,y,z) = get in 
        appendFile "analemma.txt" (show x ++ "," ++ show y ++ "," ++ show z ++ "\n")  
    
    showF:: [(Float, Float, Float)] -> IO()
    showF [] = putStrLn "This is empty"
    showF [x] = inputFile x
    showF (x:xs) = inputFile x >> showF xs
    
    main :: IO ()
    main = do
        writeFile "analemma.txt" ""
        showF showdata
    