{-
-- EPITECH PROJECT, 2023
-- Main
-- File description:
-- ImageCompressor
-}

import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Cluster (CLU (Clu), PIX (Pix))
import Parse (pArg, pAct, PARSE (Parse))
import Body (clustering)

main :: IO ()
main | (length (unsafeLocalState getArgs)) /= 6 =
      hPutStrLn stderr "USAGE: ./imageCompressor -n N -l L -f F" >>
      hPutStrLn stderr "\tN number of colors in the final image" >>
      hPutStrLn stderr "\tL convergence limit" >>
 hPutStrLn stderr "\tF path to the file containing the colors of the pixels" >>
      exitWith (ExitFailure 84)
     | clustering (pArg (pAct (unsafeLocalState getArgs)) (Parse (-1) (-1) []))
 == [] = hPutStrLn stderr "Invalid argument" >> exitWith (ExitFailure 84)
     | otherwise = mapM_ print (clustering (pArg
     (pAct (unsafeLocalState getArgs)) (Parse (-1) (-1) [])))
