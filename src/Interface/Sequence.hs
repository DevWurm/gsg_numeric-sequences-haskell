{-# LANGUAGE ScopedTypeVariables #-}

module Interface.Sequence (
  outputSequenceMenue
) where

  import Sequences.General
  import Interface.General
  import Control.Monad
  import Control.Exception

  outputSequenceMenue :: (Real a, Show a) => Sequence a -> IO ()
  outputSequenceMenue seq = do
                              putStrLn "Folge ausgeben"
                              putStrLn "<1> Bestimmtes Element ausgeben"
                              putStrLn "<2> Bestimmte Anzahl von Elementen angeben"
                              putStrLn "<3> Summe einer bestimmten Anzahl von Elementen ausgeben"
                              putStrLn "<4> Beenden"
                              putStr "Auswahl: "
                              option <- getLine
                              state <- handleOption seq option
                              when (state == Redo) $ outputSequenceMenue seq

  handleOption :: (Real a, Show a) => Sequence a -> String -> IO MenueState
  handleOption seq optionStr = do
                                 let
                                  option = read optionStr
                                 performAction seq option `catch` (\(e :: SomeException) -> return Done)

  performAction :: (Real a, Show a) => Sequence a -> Int -> IO MenueState
  performAction seq 1 = do
                          putStr "Elementnummer: "
                          elementStr <- getLine
                          let
                            element = read elementStr
                          ((print $ seq !! element) >> (return Redo)) `catch` (\(e :: SomeException) -> return Redo)
  performAction seq 2 = do
                          putStr "Anzahl von Elementen: "
                          ammountStr <- getLine
                          let
                            ammount = read ammountStr
                          ((print $ take ammount seq) >> (return Redo)) `catch` (\(e :: SomeException) -> return Redo)
  performAction seq 3 = do
                          putStr "Anzahl von Elementen: "
                          ammountStr <- getLine
                          let
                            ammount = read ammountStr
                          ((print $ sumUntil ammount seq) >> (return Redo)) `catch` (\(e :: SomeException) -> return Redo)
  performAction _ _ = return Done
