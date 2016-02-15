module Sequences.Arithmetic (
    recArSequence,
    expArSequence
) where

import Sequences.General

recArSequence :: (Real a) => a -> a -> Sequence a
recArSequence a0 p = recSequence a0 (+p)

expArSequence :: (Real a) => a -> a -> Sequence a
-- n needs to be converted to zero-based, because the first element of the result sequence is a0
expArSequence a0 p = expSequence (\n -> a0 + (fromInteger (n - 1)) * p)