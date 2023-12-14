{-
-- EPITECH PROJECT, 2023
-- Parse
-- File description:
-- ImageCompressor
-}

module Parse where

import Cluster (PIX (Pix, Null), POINT (Point), COLOR (Color))
import Text.Read (readMaybe, read)
import Foreign.Marshal.Unsafe (unsafeLocalState)

data PARSE = Empty | Parse Int Float [PIX] deriving (Eq)

instance Show PARSE where
    show (Parse i f l) = show i ++ "\n-\n" ++ show
     f ++ "\n-\n" ++ show l

getInt :: String -> Int
getInt [] = (-1)
getInt s = case readMaybe s :: Maybe Int of
                            Just _ -> read s
                            Nothing -> (-1)

getFlo :: String -> Float
getFlo [] = (-1)
getFlo s = case readMaybe s :: Maybe Float of
                            Just _ -> read s
                            Nothing -> (-1)

getTup :: String -> (Int, Int)
getTup [] =  ((-1), (-1))
getTup s = case readMaybe s :: Maybe (Int, Int) of
                            Just _ -> read s
                            Nothing ->  ((-1), (-1))

getTru :: String -> (Int, Int, Int)
getTru [] =  ((-1), (-1), (-1))
getTru s = case readMaybe s :: Maybe (Int, Int, Int) of
                            Just _ -> read s
                            Nothing -> ((-1), (-1), (-1))

parsePix :: (Int, Int) -> (Int, Int, Int) -> PIX
parsePix (xa, xb) (r, g, b) = Pix (Point (xa, xb))
 (Color (r, g, b))

parseFold :: [String] -> PIX
parseFold (x:y:[]) = parsePix (getTup x) (getTru y)
parseFold _ = Null

isPix :: PIX -> Bool
isPix Null = False
isPix (Pix (Point ((-1), (-1))) _) = False
isPix (Pix _ (Color (r, g, b))) | r >= 0 && r <= 255 && g >= 0 && g <= 255 &&
                               b >= 0 && b <= 255 = True
                              | otherwise = False

parsePixs :: [String] -> [PIX] -> [PIX]
parsePixs [] res = res
parsePixs (x:xs) res | (isPix (parseFold (words x))) == True = parsePixs xs
                        ((parseFold (words x)):res)
                     | otherwise = []

pArg :: [(String, String)] -> PARSE -> PARSE
pArg [] res = res
pArg (("-n", val):xs) (Parse _ l f) = pArg xs (Parse (getInt val) l f)
pArg (("-l", val):xs) (Parse n _ f) = pArg xs (Parse n (getFlo val) f)
pArg (("-f", val):xs) (Parse n l _) = pArg xs (Parse n l
 (parsePixs (lines (unsafeLocalState (readFile val))) []))
pArg _ _ = Empty

pAct :: [String] -> [(String, String)]
pAct [] = []
pAct (_:[]) = []
pAct (x:y:l) = (x, y):(pAct l)
