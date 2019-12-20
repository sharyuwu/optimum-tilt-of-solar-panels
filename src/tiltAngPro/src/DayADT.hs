module DayADT 
    (
    DayT,
    perihelion
    )where

import Data.Time

type DayT = Day


perihelion :: DayT -> Integer
perihelion day = 
    let (y, m, d) = toGregorian day
    in if m == 12 && d >= 21 
     then diffDays day (fromGregorian y 12 21)
     else diffDays day (fromGregorian (y-1) 12 21)
