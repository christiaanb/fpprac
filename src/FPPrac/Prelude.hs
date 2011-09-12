module FPPrac.Prelude
  ( module Prelude
  , Number
  , (!)
  , length
  , take
  , drop
  , replicate
  , code
  , decode
  , itoa
  , atoi
  , ftoa
  , atof
  )
where

import Data.Char (ord,chr)
import Prelude hiding (Int,Integer,Double,Float,length,take,drop,replicate)
import qualified Prelude as P

import FPPrac.Prelude.Number

default ()

(!) :: [a] -> Number -> a
xs ! (I i) = xs !! (fromInteger i)

length :: [a] -> Number
length = I . toInteger . P.length

nil :: [a]
nil = []

take :: Number -> [a] -> [a]
take (I i) xs = P.take (fromInteger i) xs
take _     _  = error "take undefined for float"

drop :: Number -> [a] -> [a]
drop (I i) xs = P.drop (fromInteger i) xs
drop _     _  = error "drop undefined for float"

replicate :: Number -> a -> [a]
replicate (I i) a = replicate (fromInteger i) a
replicate _     _ = error "replicate undefined for float"

code :: Char -> Number
code c = if i < 256 then
      I $ toInteger i
    else
      error $ "encode: Not an ASCII character: " ++ [c]
  where
    i = ord c

decode :: Number -> Char
decode (I i) | i < 256   = chr $ fromInteger i
             | otherwise = error $ "decode: Not an ASCII value: " ++ show i

itoa :: Number -> String
itoa = show

atoi :: String -> Number
atoi = I . read

ftoa :: Number -> String
ftoa = show

atof :: String -> Number
atof = F . read
