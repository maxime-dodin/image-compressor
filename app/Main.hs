module Main where

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

import Parse
import KmeansAlgorithme
import Lib
import RandomCentroid(randomCreate)
import PrintCluster

handleFile :: Config -> IO ()
handleFile (Conf col limit path) = do
    input <- readFile path
    let pixels = parseFile $ lines input
    centroid <- randomCreate col pixels []
    clsterTab <- kMeanLoop pixels centroid col limit
    finalShow clsterTab


main :: IO ()
main = do
    args <- getArgs
    let configuration = fillconf args startconf
    checkconfig configuration
    handleFile configuration