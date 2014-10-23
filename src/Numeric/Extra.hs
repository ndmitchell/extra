
module Numeric.Extra(
    module Numeric,
    showDP,
    intToDouble, intToFloat, floatToDouble, doubleToFloat
    ) where

import Numeric
import Control.Arrow

---------------------------------------------------------------------
-- Data.String

-- | Show a number to a fixed number of decimal places.
--
-- > showDP 4 pi == "3.1416"
-- > showDP 0 pi == "3"
-- > showDP 2 3  == "3.00"
showDP :: RealFloat a => Int -> a -> String
showDP n x = a ++ (if n > 0 then "." else "") ++ b ++ replicate (n - length b) '0'
    where (a,b) = second (drop 1) $ break (== '.') $ showFFloat (Just n) x ""


---------------------------------------------------------------------
-- Numeric

-- | Specialised numeric conversion, type restricted version of 'fromIntegral'.
intToDouble :: Int -> Double
intToDouble = fromIntegral

-- | Specialised numeric conversion, type restricted version of 'fromIntegral'.
intToFloat :: Int -> Float
intToFloat = fromIntegral

-- | Specialised numeric conversion, type restricted version of 'realToFrac'.
floatToDouble :: Float -> Double
floatToDouble = realToFrac

-- | Specialised numeric conversion, type restricted version of 'realToFrac'.
doubleToFloat :: Double -> Float
doubleToFloat = realToFrac


