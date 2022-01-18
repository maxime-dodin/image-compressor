module RandomCentroid
(
    randomCreate
) where

import System.Random
import Data.List
import Lib

randomRGB :: Pixel -> Centroid
randomRGB all@(Pixel _ _ r g b) = (Centroid r g b)

randomCreate :: Int -> [Pixel] -> [Centroid] -> IO [Centroid]
randomCreate 0 _ centroid = return centroid
randomCreate color pixels centr
    | color > (length pixels) = randomCreate ((length pixels) - 1) pixels centr
    | otherwise = do
        index <- randomRIO (0, ((length pixels) - 1))
        let value = pixels !! index
        let newPixels = delete value pixels
        cent <- randomCreate (color - 1) newPixels centr
        return ((randomRGB value) : cent)