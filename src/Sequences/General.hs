module Sequences.General (
    Sequence,
    newRecSequence,
    newExpSequence
) where

type Sequence a = [a]

newRecSequence :: (Real a) =>  a -> (a -> a) -> Sequence a
newRecSequence a0 f = a0 : map f (newRecSequence a0 f)

newExpSequence :: (Real a) => (Integer -> a) -> Sequence a
newExpSequence f = map f [1 .. ]

sumUntil :: (Real a) => Int -> Sequence a -> a
sumUntil n = sum . take n