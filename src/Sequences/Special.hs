module Sequences.Special where

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
                            intN = fromInteger n
                          in
                            (dFibs !! intN) / (dFibs !! (intN + 1)))
    where
        dFibs :: Sequence Double
        dFibs = map fromInteger fibs

fibs :: Sequence Integer
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)