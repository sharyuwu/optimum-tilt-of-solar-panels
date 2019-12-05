module Dayduration where

import Data.Time

daydurT:: Integer -> Int -> Int -> Day
daydurT = fromGregorian


setdurlist:: Day -> Day -> [Integer]
setdurlist staDay inpDay
  | staDay == inpDay = [diffDays inpDay (perihelion inpDay)]
  | staDay < inpDay = diffDays staDay (perihelion staDay) : setdurlist (addDays 1 staDay) inpDay

perihelion::Day -> Day
perihelion day = 
    let (y, m, d) = toGregorian day
    in if m == 12 && d >= 21 
     then fromGregorian y 12 21
     else fromGregorian (y-1) 12 21
