module Sequences.Special (
    gs1,
    gs2,
    z,
    f
) where

import Sequences.General

gs1 :: Sequence Double
gs1 = recSequence 1 (\n0 -> 1 / (1 + n0))

gs2 :: Sequence Double
gs2 = recSequence (1 / (sqrt 2)) (\n0 -> 1 / (sqrt (2 + n0)))

z :: Sequence Double
z = expSequence (\n -> 1 / (fromInteger . fac $ n))

fac :: Integer -> Integer
fac 1 = 1
fac n | n > 0 = n * (fac (n - 1))
      | otherwise = error "fac is only defined for Integers bigger then 0"

f :: Sequence Double
f = expSequence (\n -> let
                            {- n is starting at 1 but fibs list is starting at 0, so n needs to be converted to be
                            zero based. (!!) operator only accepts Int arguments. Because n is from type Integer
                            it needs to be converted. -}
                            zeroBasedIntN = fromInteger (n - 1) --
                       in
                            (dFibs !! zeroBasedIntN) / (dFibs !! (zeroBasedIntN + 1)))
    where
        dFibs :: Sequence Double
        dFibs = map fromInteger fibs

fibs :: Sequence Integer
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)