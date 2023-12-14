{-
-- EPITECH PROJECT, 2023
-- Body
-- File description:
-- ImageCompressor
-}

module Body where

import Cluster (pixInClu, CLU(Clu), COLOR(Color), PIX(Pix), getDistColor)
import Parse (PARSE(Empty, Parse))
import System.Random (randomR, newStdGen)
import Foreign.Marshal.Unsafe (unsafeLocalState)

myunsafeLocalState :: IO a -> a
myunsafeLocalState x = unsafeLocalState x

getMaxAndMinRed :: [PIX] -> (Int, Int) -> (Int, Int)
getMaxAndMinRed [] res = res
getMaxAndMinRed ((Pix _ (Color (res, _, _))):xs) (min, max)
                              | res > max = getMaxAndMinRed xs (min, res)
                              | res < min = getMaxAndMinRed xs (res, max)
                              | otherwise = getMaxAndMinRed xs (min, max)

getMaxAndMinGreen :: [PIX] -> (Int, Int) -> (Int, Int)
getMaxAndMinGreen [] res = res
getMaxAndMinGreen ((Pix _ (Color (_, res, _))):xs) (min, max)
                              | res > max = getMaxAndMinGreen xs (min, res)
                              | res < min = getMaxAndMinGreen xs (res, max)
                              | otherwise = getMaxAndMinGreen xs (min, max)

getMaxAndMinBlue :: [PIX] -> (Int, Int) -> (Int, Int)
getMaxAndMinBlue [] res = res
getMaxAndMinBlue ((Pix _ (Color (_, _, res))):xs) (min, max)
                              | res > max = getMaxAndMinBlue xs (min, res)
                              | res < min = getMaxAndMinBlue xs (res, max)
                              | otherwise = getMaxAndMinBlue xs (min, max)

randomCluster :: [PIX] -> CLU
randomCluster l = Clu (Color
 (fst (randomR (getMaxAndMinRed l (255, 0)) (myunsafeLocalState newStdGen)),
 fst (randomR (getMaxAndMinGreen l (255, 0)) (myunsafeLocalState newStdGen)),
 fst (randomR (getMaxAndMinBlue l (255, 0)) (myunsafeLocalState newStdGen))))
 []

kMeansPPintegration :: CLU -> [CLU] -> [CLU]
kMeansPPintegration (Clu (Color (r, g, b)) l) res =
       (Clu (Color (r, g, b)) l):(Clu (Color ((255 - r), (255 - g),
        (255 - b))) l):res

randomClusterInit :: Int -> [PIX] -> [CLU]
randomClusterInit 0 _ = []
randomClusterInit 1 l = (randomCluster l):(randomClusterInit 0 l)
randomClusterInit idx l = (kMeansPPintegration (randomCluster l)
 (randomClusterInit (idx - 2) l))

updateRed :: [PIX] -> Int
updateRed [] = 0
updateRed ((Pix _ (Color (r, _, _))):xs) = r + (updateRed xs)

updateGreen :: [PIX] -> Int
updateGreen [] = 0
updateGreen ((Pix _ (Color (_, g, _))):xs) = g + (updateGreen xs)

updateBlue :: [PIX] -> Int
updateBlue [] = 0
updateBlue ((Pix _ (Color (_, _, b))):xs) = b + (updateBlue xs)

updateColor :: [PIX] -> COLOR
updateColor l = Color ((div (updateRed l) (length l)),
 (div (updateGreen l) (length l)), (div (updateBlue l) (length l)))

updateClust :: [PIX] -> [PIX] -> CLU
updateClust [] l = randomCluster l
updateClust l _ = Clu (updateColor l) []

updateClusters :: [CLU] -> [PIX] -> [CLU]
updateClusters [] _ = []
updateClusters ((Clu _ l):xs) p = (updateClust l p):(updateClusters xs p)

diffCluster :: CLU -> CLU -> Float
diffCluster (Clu _ []) _ = 1000.0
diffCluster _ (Clu _ []) = 1000.0
diffCluster (Clu ca _) (Clu cb _) = getDistColor ca cb

isLessDiff :: Float -> [CLU] -> [CLU] -> Bool
isLessDiff _ [] [] = True
isLessDiff _ [] _ = False
isLessDiff _ _ [] = False
isLessDiff d (x:xs) (y:ys) | d > (diffCluster x y) = (isLessDiff d xs ys)
                           | otherwise = False

whileConvLim :: PARSE -> [CLU] -> [CLU] -> [CLU]
whileConvLim (Parse i f l) la lb | (isLessDiff f la lb) == False = whileConvLim
 (Parse i f l) (pixInClu (updateClusters la l) l) la
                                  | otherwise = la

clustering :: PARSE -> [CLU]
clustering Empty = []
clustering (Parse _ _ []) = []
clustering (Parse g f l) | g > 0 && f /= (-1) = reverse
 (whileConvLim (Parse g f l) (pixInClu (randomClusterInit g l) l) [])
                         | otherwise = []
