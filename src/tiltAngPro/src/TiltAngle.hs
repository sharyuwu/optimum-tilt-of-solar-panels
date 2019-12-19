module TiltAngle where
    
    import Calculation
    import Data.List
    import Data.Maybe

    getTilt:: [DegreeT] -> Double -> DegreeT
    getTilt zenList sunInt = 
        let list = map (sglSunIn sunInt) zenList in
        let max = maximum list in 
        let index = fromJust $elemIndex max list in
            last $ take index zenList

   {- ifMax :: [DegreeT] -> [Double] -> DegreeT
    ifMax [x] [y] = y
    ifMax (x:xs) (y:ys)
        | y > max = x
        | otherwise = ifMax xs ys
            where max = ifMax xs ys-}
                          
    