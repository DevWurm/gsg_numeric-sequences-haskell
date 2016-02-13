module Sequences.General (
    Sequence,
    newRecSequence,
    newExpSequence,
    sumUntil,
    partialSumSequence,
    limit
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
                              {- Creating an infinite list of functions which sum the first n elements of a list,
                                 with n starting by 1. -}
                              sumFunctions = map sumUntil [1 .. ]
                         in
                              {- Creating the infinite list of results of the sumFunctions functions applied to the
                                 provided sequence. This list is equivalent to the partial sum sequence of the provided
                                 sequence. -}
                              map ($ seq) sumFunctions

limit :: (Real a) => Sequence a -> Double -> Int -> Int -> Maybe Double
limit seq eps af nmax = let
                            ssqs = subseqs af seq
                            ssqsInRange = take nmax ssqs
                            ssqsInEps = filter (\ssq -> (listDiff ssq) > eps) ssqsInRange
                        in
                            if (null ssqsInEps) then
                                Nothing
                            else
                                Just . average . head $ ssqsInEps
                        where
                            listDiff :: (Real a) => [a] -> Double
                            listDiff xs = realToFrac ((maximum xs) - (minimum xs))

subseqs :: (Real a) => Int -> Sequence a -> [Sequence a]
subseqs n seq = (take n seq) : (subseqs n . tail $ seq)

average :: (Real a) => [a] -> Double
average xs = (realToFrac . sum $ xs) / (fromIntegral . length $ xs)