{-# LANGUAGE ScopedTypeVariables #-}

module Interface.ArGeo (
  arGeoMenue
) where

  import Interface.General
  import Interface.Sequence
  import Sequences.Arithmetic
  import Sequences.Geometric
  import Control.Monad
  import Control.Exception
  import System.IO

  arGeoMenue :: IO ()
  arGeoMenue = do
                  putStrLn "Aritmetische / Geometrische Folge ausgeben"
                  putStrLn "<1> Aritmetische Folge"
                  putStrLn "<2> Geometrische Folge"
                  putStrLn "<3> Beenden"
                  putStr "Auswahl: "
                  hFlush stdout
                  option <- getLine
                  putChar '\n'
                  state <- handleOption option
                  when (state == Redo) arGeoMenue

  handleOption :: String -> IO MenueState
  handleOption ('1':_) = do
                           outputArSequence
                           return Redo
  handleOption ('2':_) = do
                           outputGeoSequence
                           return Redo
  handleOption _ = return Done

  outputArSequence :: IO ()
  outputArSequence = do
                       putStrLn "Arithmetische Folge"
                       putStrLn "<1> Rekursive Definition"
                       putStrLn "<2> Explizite Definition"
                       putStrLn "<3> Beenden"
                       putStr "Auswahl: "
                       hFlush stdout
                       option <- getLine
                       putChar '\n'
                       case (head option) of
                         '1' -> outputRecArSequence
                         '2' -> outputExpArSequence
                         _ -> return ()

  outputRecArSequence :: IO ()
  outputRecArSequence = do
                          putStrLn "Rekursive Arithmetische Folge"
                          putStr "Parameter a0: "
                          hFlush stdout
                          a0Str <- getLine
                          putStr "Parameter p: "
                          hFlush stdout
                          pStr <- getLine
                          let
                            a0 = read a0Str
                            p = read pStr
                            output = outputSequenceMenue $ recArSequence a0 p
                          output `catch` (\(e :: SomeException) -> putStrLn "Parameter konnten nicht in Zahlen umgewandelt werden")
                          putChar '\n'

  outputExpArSequence :: IO ()
  outputExpArSequence = do
                          putStrLn "Explizite Arithmetische Folge"
                          putStr "Parameter a0: "
                          hFlush stdout
                          a0Str <- getLine
                          putStr "Parameter p: "
                          hFlush stdout
                          pStr <- getLine
                          let
                            a0 = read a0Str
                            p = read pStr
                            output = outputSequenceMenue $ expArSequence a0 p
                          output `catch` (\(e :: SomeException) -> putStrLn "Parameter konnten nicht in Zahlen umgewandelt werden")
                          putChar '\n'

  outputGeoSequence :: IO ()
  outputGeoSequence = do
                         putStrLn "Geometrische Folge"
                         putStrLn "<1> Rekursive Definition"
                         putStrLn "<2> Explizite Definition"
                         putStrLn "<3> Beenden"
                         putStr "Auswahl: "
                         hFlush stdout
                         option <- getLine
                         putChar '\n'
                         case (head option) of
                           '1' -> outputRecGeoSequence
                           '2' -> outputExpGeoSequence
                           _ -> return ()

  outputRecGeoSequence :: IO ()
  outputRecGeoSequence = do
                          putStrLn "Rekursive Geometrische Folge"
                          putStr "Parameter a0: "
                          hFlush stdout
                          a0Str <- getLine
                          putStr "Parameter q: "
                          hFlush stdout
                          qStr <- getLine
                          let
                            a0 = read a0Str
                            q = read qStr
                            output = outputSequenceMenue $ recGeoSequence a0 q
                          output `catch` (\(e :: SomeException) -> putStrLn "Parameter konnten nicht in Zahlen umgewandelt werden")
                          putChar '\n'

  outputExpGeoSequence :: IO ()
  outputExpGeoSequence = do
                            putStrLn "Explizite Geometrische Folge"
                            putStr "Parameter a0: "
                            hFlush stdout
                            a0Str <- getLine
                            putStr "Parameter q: "
                            hFlush stdout
                            qStr <- getLine
                            let
                              a0 = read a0Str
                              q = read qStr
                              output = outputSequenceMenue $ expGeoSequence a0 q
                            output `catch` (\(e :: SomeException) -> putStrLn "Parameter konnten nicht in Zahlen umgewandelt werden")
                            putChar '\n'
