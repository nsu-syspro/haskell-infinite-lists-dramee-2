{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Task2 (Stream (Stream), fromList)
import Data.Ratio (Ratio, numerator)

-- | Power series represented as infinite stream of coefficients
-- 
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (x + x ^ 2 + x ^ 4)
-- [0,1,1,0,1,0,0,0,0,0]
-- >>> coefficients ((1 + x)^5)
-- [1,5,10,10,5,1,0,0,0,0]
-- >>> coefficients (42 :: Series Integer)
-- [42,0,0,0,0,0,0,0,0,0]
--
newtype Series a = Series
  { coefficients :: Stream a
  -- ^ Returns coefficients of given power series
  --
  -- For following series
  --   @a0 + a1 * x + a2 * x^2 + ...@
  -- coefficients would be
  --   @a0, a1, a2, ...@
  }

-- | Power series corresponding to single @x@
--
-- First 10 coefficients:
--
-- >>> coefficients x
-- [0,1,0,0,0,0,0,0,0,0]
--
x :: Num a => Series a
x = Series (Stream 0 (Stream 1 (fromList 0 [])))

streamZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZipWith f (Stream a as) (Stream b bs) = Stream (f a b) (streamZipWith f as bs)


instance Num a => Num (Series a) where
  fromInteger n = Series (Stream (fromInteger n) (fromList 0 []))

  negate (Series s) = Series (fmap negate s)

  Series a + Series b = Series (streamZipWith (+) a b)

  (Series (Stream a0 a')) * b@(Series (Stream b0 b')) =
    Series (Stream (a0 * b0)
      (coefficients (a0 *: Series b' + Series a' * b)))



  abs s = s
  signum _ = 1


instance Fractional a => Fractional (Series a) where
  fromRational r = Series (Stream (fromRational r) (fromList 0 []))

  Series (Stream a0 a') / b@(Series (Stream b0 b')) =
    Series (Stream q0
      (coefficients ((Series a' - q0 *: Series b') / b)))
    where
      q0 = a0 / b0


-- | Multiplies power series by given number
-- 
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (2 *: (x + x ^ 2 + x ^ 4))
-- [0,2,2,0,2,0,0,0,0,0]
-- >>> coefficients (2 *: ((1 + x)^5))
-- [2,10,20,20,10,2,0,0,0,0]
--
infixl 7 *:
(*:) :: Num a => a -> Series a -> Series a
(*:) a (Series coeffs) = Series (fmap (a*) coeffs)

-- | Helper function for producing integer
-- coefficients from generating function
-- (assuming denominator of 1 in all coefficients)
--
-- Usage example:
--
-- >>> gen $ (2 + 3 * x)
-- [2,3,0,0,0,0,0,0,0,0]
--
gen :: Series (Ratio Integer) -> Stream Integer
gen (Series coeffs) = fmap numerator coeffs

-- | Returns infinite stream of ones
--
-- First 10 elements:
--
-- >>> ones
-- [1,1,1,1,1,1,1,1,1,1]
--
ones :: Stream Integer
ones = gen (1 / (1 - x))


-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
--
nats :: Stream Integer
nats = gen (1 / ((1 - x) * (1 - x)))

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: Stream Integer
fibs = gen (x / (1 - x - x * x))

