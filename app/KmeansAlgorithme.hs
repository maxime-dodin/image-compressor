module KmeansAlgorithme (kMeanLoop) where

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

sumPixelColors :: [Pixel] -> (Double ,Double ,Double)
sumPixelColors [pixel] = (r pixel, g pixel, b pixel)
sumPixelColors ((Pixel _ _ r1 g1 b1):pixels) = (rx, gx, bx)
    where
        (r2, g2, b2) = sumPixelColors pixels
        rx = r1 + r2
        gx = g1 + g2
        bx = b1 + b2

setColor :: [Pixel] -> Int -> Centroid
setColor [pixel] _ = (Centroid (r pixel) (g pixel) (b pixel))
setColor pixels size = (Centroid (r / (fromIntegral size))
    (g / (fromIntegral size)) (b / (fromIntegral size)))
    where
        (r, g, b) = sumPixelColors pixels

distance :: Pixel -> Centroid -> Double
distance (Pixel _ _  r g b) (Centroid r2 g2 b2) = sqrt (r'*r' + g'*g' + b'*b')
    where
      r' = r - r2
      g' = g - g2
      b' = b - b2

createClusters :: [Centroid] -> [Cluster]
createClusters = map (\centroid -> Cluster [] centroid 0)

setPixelToCentroid :: Pixel -> Centroid -> [Cluster] -> [Cluster]
setPixelToCentroid pixel c [] = []
setPixelToCentroid pixel c (h@(Cluster _ center nbPixels):t)
    | center == c = h{ pixels = pixel : pixels h, nbPixels = nbPixels + 1} : t
    | otherwise = h : setPixelToCentroid pixel c t

findMinDistCentroid :: Pixel -> [Centroid] -> Centroid
findMinDistCentroid _ [h] = h
findMinDistCentroid pixel (h:t)
    | distCentroid < distColor = h
    | otherwise = nearestCentroid
    where
        nearestCentroid = findMinDistCentroid pixel t
        distCentroid = distance pixel h
        distColor = distance pixel nearestCentroid

setPixelToClusters :: [Pixel] -> [Centroid] -> [Cluster] -> [Cluster]
setPixelToClusters pixel centroids clusters
    | length pixel == 0 = clusters
    | otherwise = setPixelToClusters (tail pixel) centroids updatedClusters
    where
        minDist = findMinDistCentroid (head pixel) centroids
        updatedClusters = setPixelToCentroid (head pixel) minDist clusters

getRemNumber :: Int -> Int -> Int
getRemNumber x y
    | x > y = y
    | otherwise = x - 1

generateNewCentroids :: [Cluster] -> [Centroid]
generateNewCentroids = map (\(Cluster pixels _ size) -> setColor pixels size)

kMeanLoop :: [Pixel] -> [Centroid] -> Int -> Double -> IO [Cluster]
kMeanLoop pixels centroids col it
    | centroids == new_centroids = return new_clusters
    | otherwise = kMeanLoop pixels new_centroids col it
    where
        default_cluster = createClusters centroids
        new_clusters = setPixelToClusters pixels centroids default_cluster
        new_centroids = generateNewCentroids new_clusters
