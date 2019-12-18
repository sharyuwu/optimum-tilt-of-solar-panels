module InputVer where

{- This module implements the Input Verification module of the MG/MIS.
-- The purpose of this module is to check that the data inputted to the system is
-- correctly typed and within their respective constraints.
-}

-- Function: verifiedLat
-- inputs: latitude:: DegreeT
-- output: Bool
-- This function should check that the input latitude is between 90 and -90 (inclusive).
verifiedLat:: DegreeT -> Bool
verifiedLat lat =
	| lat > 90 = False
	| lat < -90 = False
	| otherwise = True

-- Function: verifiedP
-- inputs: panelHeight::Float, panelWidth::Real
-- output: Bool
-- This function should check that the panel height and width are appropriate (not zero).
verifiedP:: Float Float -> Bool
verifiedP panelHeight panelWidth =
	| panelHeight > 0 && panelWidth > 0 = True
	| otherwise = False

-- Function: verifiedP
-- inputs: start::DayT, end::DayT
-- output: Bool
-- This function should check that the order of days is appropriate (start happens before end).
verifiedD:: DayT DayT -> Bool
verifiedP start end =
	| start < end = True
	| otherwise = False

