module Sequences.Arithmetric (
    newRecArSequence,
    newExpArSequence
) where

import Sequences.General

newRecArSequence :: (Real a) => a -> a -> Sequence a
newRecArSequence a0 p = newRecSequence a0 (+p)

newExpArSequence :: (Real a) => a -> a -> Sequence a
newExpArSequence a0 p = newExpSequence (\n -> a0 + n * p)