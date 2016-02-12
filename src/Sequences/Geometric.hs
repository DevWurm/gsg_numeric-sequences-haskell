module Sequences.Geometric (
    newRecGeoSequence,
    newExpGeoSequence
) where

import Sequences.General

newRecGeoSequence :: (Real a) => a -> a -> Sequence a
newRecGeoSequence a0 q = newRecSequence a0 (*q)

newExpGeoSequence :: (Real a) => a -> a -> Sequence a
newExpGeoSequence a0 q = newExpSequence (\n -> a0 * q^n)
