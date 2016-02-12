module Sequences.General (
    Sequence,
    newRecSequence,
    newExpSequence,
    sumUntil,
    partialSumSequence
) where

type Sequence a = [a]

newRecSequence :: (Real a) =>  a -> (a -> a) -> Sequence a
newRecSequence a0 f = a0 : map f (newRecSequence a0 f)

newExpSequence :: (Real a) => (Integer -> a) -> Sequence a
newExpSequence f = map f [1 .. ]

sumUntil :: (Real a) => Int -> Sequence a -> a
sumUntil n = sum . take n

partialSumSequence :: (Real a) => Sequence a -> Sequence a
partialSumSequence seq = let
                              {- Creating an infinite list of functions which sum the first n elements of a list.
                                 n is beginning by 1. -}
                              sumFunctions = map (\n -> sum . take n) [1 .. ]
                         in
                              {- Creating the infinite list of results of the sumFunctions functions applied to the
                                 provided sequence. This list is equivalent to the partial sum sequence of the provided
                                 sequence. -}
                              map ($ seq) sumFunctions