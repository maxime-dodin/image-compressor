module PrintCluster (finalShow) where

import System.Environment
import System.Directory
import System.Exit
import System.IO
import Text.Read
import System.Exit(exitWith , ExitCode(ExitFailure))
import Control.Monad
import Control.Exception
import Data.String
import Data.List ( (\\) )
import System.Random
import Data.List

import Lib

showPix :: [Pixel] -> IO ()
showPix [] = return ()
showPix ((Pixel x y r g b):xs) = putStrLn ("(" ++ show (round x) ++ ","
    ++ show (round y) ++ ") " ++ "(" ++ show (round r) ++ ","
    ++ show (round g) ++ "," ++ show (round b) ++ ")") >> showPix xs

showCb :: Double -> IO ()
showCb b = putStr (show (round b) ++ ")" ++ "\n-\n")

showCg :: Double -> IO ()
showCg g = putStr (show (round g) ++ ",")

showCr :: Double -> IO ()
showCr r = putStr ("--\n" ++ "(" ++ show (round r) ++ ",")

showLine :: [Pixel] -> Centroid -> IO ()
showLine tab (Centroid r g b) = showCr r >> showCg g >> showCb b >> showPix tab

finalShow :: [Cluster] -> IO ()
finalShow [] = return ()
finalShow ((Cluster tab c _):xs) = showLine (nub tab) c >> finalShow xs