module Sequences.General where

type Sequence a = [a]

newRecSequence :: (Real a) => (a -> a) -> a -> Sequence a
newRecSequence f a0 = a0 : map f (newRecSequence f a0)

newExpSequence :: (Real a) => (Integer -> a) -> Sequence a
newExpSequence f = map f [1 .. ]