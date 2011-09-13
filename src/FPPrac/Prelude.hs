-- | The 'FPPrac.Prelude' defines the 'Number' type (which is like Amanda's 
-- 'num' type), and hides the intricacies of Haskell's Type Classes
-- from new users when dealing with number. Also defines corresponding 
-- 'Prelude' functions that use this new 'Number' type.
module FPPrac.Prelude
  ( module Prelude
  , Number
	, atoi
	, atof
	, length
  , (!!)
	, replicate
  , take
  , drop
  , splitAt
  )
where

import Prelude hiding (Int,Integer,Double,Float,length,(!!),replicate,take,
	drop,splitAt)
import qualified Prelude as P

import FPPrac.Prelude.Number

default ()
infixl 9 !!

-- | /O(n)/. 'length' returns the length of a finite list as a 'Number'.
length :: [a] -> Number
length = I . toInteger . P.length

-- | List index (subscript) operator, starting from 0.
(!!) :: [a] -> Number -> a
xs !! (I i) = xs P.!! (fromInteger i)

-- | 'replicate' @n x@ is a list of length @n@ with @x@ the value of
-- every element.
--
-- Fails when @n@ is not an integral number
replicate :: Number -> a -> [a]
replicate (I i) a = P.replicate (fromInteger i) a
replicate _     _ = error "replicate undefined for float"

-- | 'take' @n@, applied to a list @xs@, returns the prefix of @xs@
-- of length @n@, or @xs@ itself if @n > 'length' xs@:
--
-- > take 5 "Hello World!" == "Hello"
-- > take 3 [1,2,3,4,5] == [1,2,3]
-- > take 3 [1,2] == [1,2]
-- > take 3 [] == []
-- > take (-1) [1,2] == []
-- > take 0 [1,2] == []
--
-- Fails when @n@ is not an integral number
take :: Number -> [a] -> [a]
take (I i) xs = P.take (fromInteger i) xs
take _     _  = error "take undefined for float"

-- | 'drop' @n xs@ returns the suffix of @xs@
-- after the first @n@ elements, or @[]@ if @n > 'length' xs@:
--
-- > drop 6 "Hello World!" == "World!"
-- > drop 3 [1,2,3,4,5] == [4,5]
-- > drop 3 [1,2] == []
-- > drop 3 [] == []
-- > drop (-1) [1,2] == [1,2]
-- > drop 0 [1,2] == [1,2]
--
-- Fails when @n@ is not an integral number
drop :: Number -> [a] -> [a]
drop (I i) xs = P.drop (fromInteger i) xs
drop _     _  = error "drop undefined for float"

-- | 'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
-- length @n@ and second element is the remainder of the list:
--
-- > splitAt 6 "Hello World!" == ("Hello ","World!")
-- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
-- > splitAt 1 [1,2,3] == ([1],[2,3])
-- > splitAt 3 [1,2,3] == ([1,2,3],[])
-- > splitAt 4 [1,2,3] == ([1,2,3],[])
-- > splitAt 0 [1,2,3] == ([],[1,2,3])
-- > splitAt (-1) [1,2,3] == ([],[1,2,3])
--
-- It is equivalent to @('take' n xs, 'drop' n xs)@ when @n@ is not @_|_@
-- (@splitAt _|_ xs = _|_@).
--
-- Fails when @n@ is not an integral number
splitAt :: Number -> [a] -> ([a],[a])
splitAt (I i) xs = P.splitAt (fromInteger i) xs
splitAt _     _  = error "splitAt undefined for float"
