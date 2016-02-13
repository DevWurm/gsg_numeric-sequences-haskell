module Sequences.Geometric (
    recGeoSequence,
    expGeoSequence
) where

import Sequences.General

recGeoSequence :: (Real a) => a -> a -> Sequence a
recGeoSequence a0 q = recSequence a0 (*q)

expGeoSequence :: (Real a) => a -> a -> Sequence a
expGeoSequence a0 q = expSequence (\n -> a0 * q^n)
