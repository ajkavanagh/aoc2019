module Lib
    ( libmain
    ) where

-- for argument parsing
import           System.Environment (getArgs)
import           System.Exit        (ExitCode (..), exitWith)

-- The individual days
import           Day01.M01          (main01)
import           Day01.M02          (main02)
import           Day02.M03          (main03)
import           Day02.M04          (main04)
import           Day03.M05          (main05)
import           Day03.M06          (main06)
import           Day04.M07          (main07)
import           Day04.M08          (main08)
import           Day05.M09          (main09)
import           Day05.M10          (main10)
import           Day06.M11          (main11)
import           Day06.M12          (main12)
import           Day07.M13          (main13)
import           Day07.M14          (main14)
import           Day08.M15          (main15)
import           Day08.M16          (main16)


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
        "5-1" -> main09
        "5-2" -> main10
        "6-1" -> main11
        "6-2" -> main12
        "7-1" -> main13
        "7-2" -> main14
        "8-1" -> main15
        "8-2" -> main16
        _ -> do
            putStrLn "Didn't recognise that day code.  Must be in format 1-1"
            exitWith (ExitFailure 1)
