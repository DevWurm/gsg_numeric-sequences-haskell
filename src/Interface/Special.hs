{-# LANGUAGE ScopedTypeVariables #-}

module Interface.Special (
  specialMenue
) where

  import Interface.General
  import Interface.Sequence
  import Sequences.Special
  import Control.Monad

  specialMenue :: IO ()
  specialMenue = do
                   putStrLn "Spezielle Zahlenfolgen ausgeben"
                   putStrLn "<1> Folge gs1"
                   putStrLn "<2> Folge gs2"
                   putStrLn "<3> Folge z"
                   putStrLn "<4> Folge f"
                   putStrLn "<5> Beenden"
                   putStr "Auswahl: "
                   option <- getLine
                   putChar '\n'
                   state <- handleOption option
                   when (state == Redo) specialMenue

  handleOption :: String -> IO MenueState
  handleOption ('1':_) = do
                           outputSequenceMenue gs1
                           return Redo
  handleOption ('2':_) = do
                           outputSequenceMenue gs2
                           return Redo
  handleOption ('3':_) = do
                        outputSequenceMenue z
                        return Redo
  handleOption ('4':_) = do
                        outputSequenceMenue f
                        return Redo
  handleOption _ = return Done
