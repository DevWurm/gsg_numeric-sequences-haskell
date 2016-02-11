module Sequences.General (
    Sequence,
    newRecSequence,
    newExpSequence
) where

type Sequence a = [a]

newRecSequence :: (Real a) =>  a -> (a -> a) -> Sequence a
newRecSequence a0 f = a0 : map f (newRecSequence f a0)

newExpSequence :: (Real a) => (Integer -> a) -> Sequence a
newExpSequence f = map f [1 .. ]