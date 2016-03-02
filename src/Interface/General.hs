module Interface.General (
  MenueState(..)
) where

  data MenueState = Done | Redo
  instance Eq MenueState where
    Done == Done = True
    Redo == Redo = True
    x == y = False
