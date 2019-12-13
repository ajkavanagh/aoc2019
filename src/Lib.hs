module Lib
    ( libmain
    ) where

-- for argument parsing
import System.Environment (getArgs)
import           System.Exit               (ExitCode (..), exitWith)

-- The individual days
import Day01.M01 (main01)
import Day01.M02 (main02)
import Day02.M03 (main03)
import Day02.M04 (main04)
import Day03.M05 (main05)
import Day03.M06 (main06)
import Day04.M07 (main07)
import Day04.M08 (main08)


libmain :: IO ()
libmain = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Must pass the day number in 1-1 format"
            exitWith (ExitFailure 1)
        (x:xs) -> runWith x


runWith :: String -> IO ()
runWith cmd =
    case cmd of
        "1-1" -> main01
        "1-2" -> main02
        "2-1" -> main03
        "2-2" -> main04
        "3-1" -> main05
        "3-2" -> main06
        "4-1" -> main07
        "4-2" -> main08
        _ -> do
            putStrLn "Didn't recognise that day code.  Must be in format 1-1"
            exitWith (ExitFailure 1)
