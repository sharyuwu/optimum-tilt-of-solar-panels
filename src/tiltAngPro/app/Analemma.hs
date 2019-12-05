module Analemma where
{-
    This is the scr code to calculate analemma.txt and analemmaRE.txt
    The mathematic therory can be found in "https://arxiv.org/pdf/1302.0765.pdf"
    analemmaRE.txt record the data of the values of ~rE, the date should match 
    with TABLE III of the external resource in page 11.
    analemma.txt record the data of sun declication angles. analemma.txt
    will be input to Main.hs. 
    The mathematic therory of calculating ~rE to sun declication angles
    can be found external resource in page 12 Eqs 35.
-}
    import Data.Time

{- 
 Calculated values of ~rE from the numerical integration of Earth’s 
 orbit. Its mathematical therory can be found in the external resource 
 Page 11.
-}
    x0:: Integer ->Float
    x0 d = let fd = fromInteger (negate d) / 365.25 * 360 * 0.0174533 in
        negate 0.9833 * sin fd
    
    y0:: Integer ->Float
    y0 d = let fd = fromInteger (negate d) / 365.25 * 360 * 0.0174533 in
        negate 0.9833 * cos fd
    
    y0':: Integer ->Float
    y0' d = y0 d * cos (23.44 * 0.0174533)
    
    z0:: Integer ->Float
    z0 d = negate $ y0 d * sin (23.44 * 0.0174533)

 -- |baseDate means the day of the perihelion for Earth does not occur
    baseDate:: Day
    baseDate = fromGregorian 2019 12 21

 -- |endDate means 366 days after the baseDate.
    endDate:: Day -- :: (year,month,day)
    endDate = addDays 366 baseDate
  
 -- |nextDate means get one day after the input day.
    nextDate:: Day -> Day -- :: (year,month,day)
    nextDate = addDays 1

{-
relist and re function calculate the value of the re from 
baseDate to the endDate.
-}
    
-- |If the input Day d is equal to the endDate the function terminate.
-- |Otherwise appending the result value from function"re" to the list.
-- |Then recursively call relist until its termination
    relist:: Day -> [(Float, Float, Float)] -> [(Float, Float, Float)]
    relist d x
       | d == endDate = re 366 : x
       | otherwise = re (diffDays d baseDate) : relist (nextDate d) x

-- |Calculate the ~rE value using function x0, y0 and z0.
    re:: Integer -> (Float, Float, Float)
    re daydif = (x0 daydif, y0' daydif, z0 daydif)

-- |Output the value ~rE from function relist
-- |[re 1] is the initialization of the list. Therefor, function relist
-- |start appending the list from 2 days after the baseDate
    showRe:: [(Float, Float, Float)]
    showRe = relist (addDays 2 baseDate) [re 1]

{-
Function toAnalemmareTxt and writeRetoTheFile are writing
data from showRe to the designated file, "analemmaRE.txt".
-}
    toAnalemmaReTxt:: (Float, Float, Float) -> IO()
    toAnalemmaReTxt get = let (x,y,z) = get in 
        appendFile "analemmaRE.txt" (show x ++ "," ++ show y ++ "," ++ show z ++ "\n")  
    
    writeRetoTheFile:: [(Float, Float, Float)] -> IO()
    writeRetoTheFile [] = putStrLn "This is empty"
    writeRetoTheFile [x] = toAnalemmaReTxt x
    writeRetoTheFile (x:xs) = toAnalemmaReTxt x >> writeRetoTheFile xs

{-
Function setdec and setdec are trainfering the ~rE to the sun
declination angle. The mathematic therory can be found in the
external resourse Eqs 35, Page 12.
-}

-- |Function setdec will use function getdec map through the 
-- |entire input list    
    setdec:: [(Float, Float, Float)] -> [Float]
    setdec = map getdec 

-- |Function getdec will calculate the sun declination from 
-- |the value ~rE 
    getdec:: (Float, Float, Float) -> Float
    getdec a = let (x, y, z) = a in 
        (asin z/sqrt(x^2 + y^2 + z^2)) * 57.2958

{-
Function toAnalemmaTxt and writedectoTheFile are writing
data from setdec to the designated file, "analemma.txt".
-}
    toAnalemmaTxt:: Float -> IO()
    toAnalemmaTxt get = appendFile "analemma.txt" (show get ++ "\n") 

    writedectoTheFile:: [Float] -> IO()
    writedectoTheFile [] = putStrLn "This is empty"
    writedectoTheFile [x] = toAnalemmaTxt x
    writedectoTheFile (x:xs) = toAnalemmaTxt x >> writedectoTheFile xs

    
{-
main function will rewrite the file "analemmaRE.txt" and "analemma.txt"
to an empty file.
Then call the function writeRetoTheFile and writedectoTheFile to write
the data from showRe to the designated file.
-}    
    main :: IO ()
    main = do
        writeFile "analemmaRE.txt" ""
        writeRetoTheFile showRe
        writeFile "analemma.txt" ""
        writedectoTheFile $ setdec showRe
    