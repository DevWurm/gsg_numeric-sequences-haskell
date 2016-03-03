module Interface.Top (
    topMenue
) where

import Interface.General
import Interface.ArGeo
import Interface.Special
import Control.Monad
import System.IO

topMenue :: IO ()
topMenue = do
               putStrLn "<1> Operationen mit arithmetischen und geometrischen Zahlenfolgen"
               putStrLn "<2> Operationen mit speziellen Zahlenfolgen"
               putStrLn "<3> Beenden"
               putStr "Menüpunkt: "
               hFlush stdout
               option <- getLine
               putChar '\n'
               state <- handleOption option
               when (state == Redo) topMenue

handleOption :: String -> IO MenueState
handleOption ('1':_) = do
                         arGeoMenue
                         return Redo
handleOption ('2':_) = do
                         specialMenue
                         return Redo
handleOption _ = return Done
