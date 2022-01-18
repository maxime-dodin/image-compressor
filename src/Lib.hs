module Lib
    (
    Config (..),
    Pixel(..),
    Centroid(..),
    Cluster(..)
    ) where

data Pixel = Pixel {
    posX :: Double,
    posY :: Double,
    r :: Double,
    g :: Double,
    b :: Double
} deriving (Show, Eq)

data Centroid = Centroid {
    centR :: Double,
    centG :: Double,
    centB :: Double
} deriving (Show, Eq)

data Cluster = Cluster {
    pixels :: [Pixel],
    center :: Centroid,
    nbPixels :: Int
} deriving (Show, Eq)

data Config =  Conf {
    nb_color :: Int,
    conv_lim :: Double,
    path_name :: String
} deriving (Show, Eq)
