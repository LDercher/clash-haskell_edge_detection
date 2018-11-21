module VECT2VECT where

import Clash.Prelude


topEntity :: Vec 10 Integer -> Vec 10 Integer
topEntity arr  = map (\x -> x + 1) arr

