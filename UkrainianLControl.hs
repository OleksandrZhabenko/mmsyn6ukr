-- |
-- Module      :  UkrainianLControl
-- Copyright   :  (c) OleksandrZhabenko 2019-2020
-- License     :  MIT
-- Maintainer  :  olexandr543@yahoo.com
--
-- A program and a library that can be used as a musical instrument synthesizer or for Ukrainian speech synthesis 
-- especially for poets, translators and writers. 
--

module UkrainianLControl (
  -- * Control the program
  genControl,
  -- * Security and Limits
  nSymbols
) where

import Data.Char (isDigit)
import qualified Data.Vector as V (generate)
import CaseBi (getBFst')

-- | Function that converts the first digit in the command line argument (starting the argument or being the second one after the letter character) given, 
-- which is a digit in the range @[0..9]@ (providing an ascending approximately exponential scale with a basis of 10 starting from 2 and ending at 1000000001), 
-- to the upper bound of number of symbols that the 'main' function of the @mmsyn6ukr@ executable reads from the 'System.IO.stdin' for sounding.
-- The default resulting value (no input) is 31416. If there is another first command line argument then the program 
-- terminates with the informational message. Using the command line argument is done for the security reasons: 
-- because of performative writing to the resulting file(s) there is a need to limit the used memory. For most cases it is
-- enough to use the default value. If you have enough resources and a large Ukrainian text amount then specify the higher values 
-- (5 or a greater one). 
nSymbols :: String -> Int
nSymbols xs | null xs = 31416::Int
            | otherwise = getBFst' (31416::Int, V.generate 10 (\n -> (n, (10^n + 1)::Int))) (let temp = read xs::Int in if temp <= 9 && temp >= 0
                            then temp 
                            else error "Please, specify a digit as a command line argument for the program!")

-- | Function that prepares arguments for the controlling functions for the executable @mmsyn6ukr@. It takes a first command line argument and makes 
-- an analysis to produce a set of String. The first resulting String is an argument to 'nSymbols' function, the first in the inner tuple is an argument
-- to the compression level for the comressed formats and the last one is the resulting file extension. The default value (no command line arguments) is
-- @("", ("", ".wav"))@. Please, specify the command line argument (if needed) in the form \"ABC\""
-- where A is either a letter \'f\', \'o\', \'w\' or a digit and B and C are both digits (or something equivalent, see below). 
-- 
-- Their meaning:
-- 
-- A:
-- 
-- \'f\' -> native FLAC format with compression from 0 (least) to 8 (best compression ratio) specified by the third characters; \'9\' is equivalent to \'8\'. This format is optional so, 
-- please, check whether it is supported by your SoX binaries. If no, install the SoX with support for the format. For more information, please, refer to the @sox@ documentation.
-- 
-- \'o\' -> Ogg Vorbis format with compression from -1 (best) to 10 (least) specified by the characters after the first two characters. The default value is "-1". This format is optional 
-- so, please, check whether it is supported by your SoX binaries. If no, install the SoX with support for the format. For more information, please, refer to the @sox@ documentation.
-- 
-- \'w\' -> WAV format with two options for rate - 11025 Hz if the third character is less than '5' and greater than '0' and otherwise 22050 Hz (the default one also for no command line arguments). 
-- 
-- If A is a digit, then it is used accordingly to 'nSymbols' function and SoX (if properly installed) quickly converts the .raw file to the default .wav with 22050 Hz rate.
-- 
-- To obtain the best compression ratio, please specify something like \"o9-1\" or \"o5-1\" (or similar). For the best lossless compression - \"f98\" or \"f58\" (or similar). 
-- 
-- For more information, please, see the @sox@ manuals (e. g. for @soxformat@).
genControl :: String -> (String, (String, String))
genControl (x:xs) | x == 'f' = ([head xs], ("-C" ++ (if (compare (tail xs) "9" /= GT) && (compare (tail xs) "0" /= LT) then take 1 . tail $ xs else "8"), ".flac"))
                  | x == 'o' = ([head xs], ("-C" ++ (if ((compare (tail xs) "9" /= GT) && (compare (tail xs) "0" /= LT)) then take 1 . tail $ xs else if (tail xs == "10") then "10" else "-1"), ".ogg"))
                  | x == 'w' = ([head xs], ("-r" ++ (if ((compare (tail xs) "4" /= GT) && (compare (tail xs) "0" /= LT)) then "11025" else "22050"), ".wav"))
                  | isDigit x = ([x], ("", ".wav"))
                  | otherwise = ("", ("", ".wav"))
genControl [] = ("", ("", ".wav"))
