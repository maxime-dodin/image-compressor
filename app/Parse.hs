module Parse (checkconfig, fillconf, parseFile, startconf) where

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

import Lib

startconf :: Config
startconf = Conf 0 0 []

err :: String -> IO()
err str = putStrLn str >> (exitWith $ ExitFailure 84)

replaceSpecialChar :: Char -> Char
replaceSpecialChar ',' = ' '
replaceSpecialChar c = c

parseFile :: [String] -> [Pixel]
parseFile file = map setPixel file

setPixel :: String -> Pixel
setPixel l = Pixel {posX = p1, posY = p2, r = c1, g = c2, b = c3}
        where
            positionOnly = map replaceSpecialChar ((words l !! 0) \\ "()")
            colorOnly = map replaceSpecialChar ((words l !! 1) \\ "()")
            p1 = readFromFile $ (words positionOnly) !! 0
            p2 = readFromFile $ (words positionOnly) !! 1
            c1 = readFromFile $ words colorOnly !! 0
            c2 = readFromFile $ words colorOnly !! 1
            c3 = readFromFile $ words colorOnly !! 2

checkcolor :: Config -> String -> Config
checkcolor (Conf col lim path) str = (Conf color lim path)
    where
        color = case (readMaybe str) of Nothing -> (-84); Just a -> a

checklimit :: Config -> String -> Config
checklimit (Conf col _ path) str = (Conf col limit path)
    where
        limit = case (readMaybe str) of Nothing -> (-84); Just a -> a

checkpath :: Config -> String -> Config
checkpath (Conf col lim _) str = (Conf col lim str)

checkconfig :: Config -> IO()
checkconfig (Conf col lim path)
    | col < 1 = err "error: number of colors must be greater than 0"
    | lim < 0 = err "error: convergence limit must be a positive number"
    | col == (-84) = err "error: number of colors must be a number"
    | lim == (-84) = err "error: convergence limit must be a number"
    | otherwise = return ()

fillconf :: [String] ->  Config -> Config
fillconf [] startconf = startconf
fillconf ("-n":s:t) startconf = fillconf t $ checkcolor startconf $ s
fillconf ("-l":s:t) startconf = fillconf t $ checklimit startconf $ s
fillconf ("-f":s:t) startconf = fillconf t $ checkpath startconf $ s
fillconf _ (Conf col lim path) = (Conf 0 0 [])

readFromFile :: [Char] -> Double
readFromFile c = (read c :: Double)