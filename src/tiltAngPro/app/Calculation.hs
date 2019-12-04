module Calculation where
    import Data.Angle

    sunInten:: Float
    sunInten = 1.35

    sunIntenSingle:: Float -> Float -> Float
    sunIntenSingle e z = sunInten * (1.0 / e) ** (1.0/cos z * pi/180)

    sunIntenSum:: Float -> [Float] -> Float
    sunIntenSum e = foldr ((+) . sunIntenSingle e) 0

    

