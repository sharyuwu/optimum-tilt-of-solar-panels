module Analemma where
{-
    This is the SCR code to calculate analemma.txt and analemmaRE.txt
    The mathematic theory can be found in "https://arxiv.org/pdf/1302.0765.pdf"
    analemmaRE.txt record the data of the values of ~rE, the date should match with TABLE III of the external resource on page 11.
    analemma.txt records the data of sun declination angles. analemma.txt
    will be input to Main.hs. 
    The mathematic theory of calculating ~rE to sun declination angles
    can be found in the external resource on page 12 Eqs 35.
-}
    import Data.Time

    type FP = Double -- could be changed in this single place to |Double| later
    fd :: Integer -> FP
    fd d = fromInteger (negate d) / 365.25 * 360 * 0.0174533

    baseCaseForVector = negate 0.9833
{- 
 Calculated values of ~rE from the numerical integration of Earth’s 
 orbit. Its mathematical theory can be found in the external resource 
 Page 11.
-}
    x0, y0 :: Integer -> FP
    x0 d = baseCaseForVector * sin (fd d)
    y0 d = baseCaseForVector * cos (fd d)
    
    y0':: Integer ->FP
    y0' d = y0 d * cos (23.44 * 0.0174533)
    
    z0:: Integer ->FP
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
    relist :: Day -> [(FP, FP, FP)] -> [(FP, FP, FP)]
    relist d x
       | d == endDate  = re 366 : x
       | otherwise     = re (diffDays d baseDate) : relist (nextDate d) x

-- |Calculate the ~rE value using function x0, y0 and z0.
    re :: Integer -> (FP, FP, FP)
    re daydif = (x0 daydif, y0' daydif, z0 daydif)
          -- V.map ($ daydif) xyz
-- |Output the value ~rE from function relist
-- |[re 1] is the initialization of the list. Therefore, function relist
-- |start appending the list from 2 days after the baseDate
    showRe :: [(FP, FP, FP)]
    showRe = relist (addDays 2 baseDate) [re 1]

{-
Function writeToAnalemmaReFile and writeRetoTheFile are writing
data from showRe to the designated file, "analemmaRE.txt".
-}
    writeToAnalemmaReFile:: (FP, FP, FP) -> IO()
    writeToAnalemmaReFile get = let (x,y,z) = get in 
        appendFile "analemmaRE.txt" (show x ++ "," ++ show y ++ "," ++ show z ++ "\n")  
    
    writeRetoTheFile:: [(FP, FP, FP)] -> IO ()
    writeRetoTheFile [] = putStrLn "This is empty"
    writeRetoTheFile [x] = writeToAnalemmaReFile x
    writeRetoTheFile (x:xs) = writeToAnalemmaReFile x >> writeRetoTheFile xs

{-
Function setdec and getdec are transferring the ~rE to the sun
declination angle. The mathematic theory can be found in the
external resource Eqs 35, Page 12.
-}

-- |Function setdec will use function getdec map through the 
-- |entire input list    
    setdec:: [(FP, FP, FP)] -> [FP]
    setdec = map getdec 

-- |Function getdec will calculate the sun declination from 
-- |the value ~rE 
    getdec:: (FP, FP, FP) -> FP
    -- getdec a = let (x, y, z) = a in 
        --(asin z/sqrt(x^2 + y^2 + z^2)) * 57.2958
    getdec a@ (x, y, z) = (asin z/sqrt (x^2 + y^2 + z^2)) * 57.2958

{-
Function writetoAnalemmaFile and writedectoTheFile are writing
data from setdec to the designated file, "analemma.txt".
-}
    writetoAnalemmaFile:: FP -> IO()
    writetoAnalemmaFile get = appendFile "analemma.txt" (show get ++ "\n") 

    writedectoTheFile:: [FP] -> IO() -- mapM_
    writedectoTheFile [] = putStrLn "This is empty"
    writedectoTheFile [x] = writetoAnalemmaFile x
    writedectoTheFile (x:xs) = writetoAnalemmaFile x >> writedectoTheFile xs

    -- |w = writeFile fileName . unlines . map show
{-
main function will write the file "analemmaRE.txt" and "analemma.txt"
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
    