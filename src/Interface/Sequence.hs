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
                              putStrLn "<4> Grenzwert suchen"
                              putStrLn "<5> Beenden"
                              putStr "Auswahl: "
                              option <- getLine
                              state <- performAction seq option
                              when (state == Redo) $ outputSequenceMenue seq

  performAction :: (Real a, Show a) => Sequence a -> String -> IO MenueState
  performAction seq ('1':_) = do
                                putStr "Elementnummer: "
                                elementStr <- getLine
                                let
                                  element = read elementStr
                                  output = do
                                              print $ seq !! element
                                              return Redo
                                output `catch` (\(e :: SomeException) -> return Redo)
  performAction seq ('2':_) = do
                                putStr "Anzahl von Elementen: "
                                ammountStr <- getLine
                                let
                                  ammount = read ammountStr
                                  output = do
                                              print $ take ammount seq
                                              return Redo
                                output `catch` (\(e :: SomeException) -> return Redo)
  performAction seq ('3':_) = do
                                putStr "Anzahl von Elementen: "
                                ammountStr <- getLine
                                let
                                  ammount = read ammountStr
                                  output = do
                                              print $ sumUntil ammount seq
                                              return Redo
                                output `catch` (\(e :: SomeException) -> return Redo)
  performAction seq ('4':_) = do
                                putStr "Größe der Epsilonumgebung: "
                                epsStr <- getLine
                                putStr "Minimale Anzahl von Elementen inerhalb der Epsilonumgebung: "
                                nfStr <- getLine
                                putStr "Maximale Anzahl an durchsuchten Elementen: "
                                nmaxStr <- getLine
                                let
                                  eps = read epsStr
                                  nf = read nfStr
                                  nmax = read nmaxStr
                                  output = do
                                             print $ limit seq eps nf nmax
                                             return Redo
                                output `catch` (\(e :: SomeException) -> return Redo)
  performAction _ _ = return Done
