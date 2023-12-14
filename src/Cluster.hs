{-
-- EPITECH PROJECT, 2023
-- Cluster
-- File description:
-- ImageCompressor
-}

module Cluster where

import Data.List (intercalate)
data POINT = Point (Int, Int) deriving (Eq)
data COLOR = Color (Int, Int, Int) deriving (Eq)
data PIX = Null | Pix POINT COLOR deriving (Eq)
data CLU = Clu COLOR [PIX] deriving (Eq)

instance Show POINT where
    show (Point d) = show d

instance Show COLOR where
    show (Color d) = show d

instance Show PIX where
    show (Pix p c) = show p ++ " " ++ show c

instance Show CLU where
    show (Clu c l) = "--\n" ++ show c ++ "\n-\n" ++ intercalate "\n"
     (map show l)

getDistColorCal :: (Float, Float, Float) -> (Float, Float, Float) -> Float
getDistColorCal (xr, xg, xb) (yr, yg, yb) = sqrt((xr - yr)^2 + (xg - yg)^2
 + (xb - yb)^2)

getDistColor :: COLOR -> COLOR -> Float
getDistColor (Color (xr, xg, xb)) (Color (yr, yg, yb)) = getDistColorCal
 (fromIntegral xr, fromIntegral xg, fromIntegral xb)
 (fromIntegral yr, fromIntegral yg, fromIntegral yb)

getMostNear :: PIX -> [CLU] -> COLOR
getMostNear _ [] = Color (1000, 1000, 1000)
getMostNear (Pix c (Color (r, g, b))) ((Clu x _):xs)
 | (getDistColor (Color (r, g, b)) x) < (getDistColor (Color (r, g, b))
  (getMostNear (Pix c (Color (r, g, b))) xs)) = x
 | otherwise = getMostNear (Pix c (Color (r, g, b))) xs

appendW :: [a] -> a -> [a]
appendW l s = reverse (s:(reverse l))

addMostNear :: [CLU] -> PIX -> COLOR -> [CLU]
addMostNear [] _ _ = []
addMostNear _ _ (Color (1000, 1000, 1000)) = []
addMostNear ((Clu x l):xs) p c | x == c = (Clu x (appendW l p)):xs
                               | otherwise = (Clu x l):(addMostNear xs p c)

pixInClu :: [CLU] -> [PIX] -> [CLU]
pixInClu res [] = res
pixInClu [] _ = []
pixInClu res (x:xs) = pixInClu (addMostNear res x (getMostNear x res)) xs
