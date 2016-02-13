module Sequences.Arithmetric (
    recArSequence,
    expArSequence
) where

import Sequences.General

recArSequence :: (Real a) => a -> a -> Sequence a
recArSequence a0 p = recSequence a0 (+p)

expArSequence :: (Real a) => a -> a -> Sequence a
expArSequence a0 p = expSequence (\n -> a0 + (fromInteger n) * p)