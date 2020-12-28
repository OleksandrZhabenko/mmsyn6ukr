-- |
-- Module      :  Melodics.Executable
-- Copyright   :  (c) OleksandrZhabenko 2019-2020
-- License     :  MIT
-- Maintainer  :  olexandr543@yahoo.com
--
-- A program and a library that can be used as a musical instrument synthesizer or for Ukrainian speech synthesis
-- especially for poets, translators and writers.
--

module Melodics.Executable (
  circle
  , workWithInput
  , rawToSoundFile
  , printInfoF
  , recFileName
)
where

import Data.Char (isSpace, isControl)
import Data.Maybe (isJust,fromJust)
import System.IO
import System.Process (callProcess)
import System.Directory (removeFile)
import Control.Exception (onException)
import EndOfExe (showE)
import Melodics.Ukrainian (appendS16LEFile, convertToProperUkrainian)
import UkrainianLControl

-- | Is used to repeat the cycle of creation of the sound files in the current directory for the @mmsyn6ukr@  executable.
circle :: String -> IO ()
circle zs = onException (mapM_ (workWithInput zs) [1..]) (do
    putStr "Notice, there was (may be) CmdLineArgument exception. To avoid it, please, specify the command line argument (if needed) in the form \"ABC\""
    putStr " where A is either a letter \'f\', \'o\', \'w\' or a digit and B and C are both digits! The exception (may be) arose from "
    putStrLn $ "the command line arguments " ++ show zs ++ ". Please, check also whether the SoX was installed with the support for needed codec.")

-- | Interactively creates sound files in the current directory for the Ukrainian text input. Is used internally in the 'circle'
workWithInput :: String -> Int -> IO ()
workWithInput zs _ = do
  [nameSF,ys] <- nameAndControl zs [1,2]
  withBinaryFile (nameSF ++ ".raw") AppendMode (appendS16LEFile (convertToProperUkrainian ys))
  putStrLn "The .raw file was created by the program. If there is SoX installed then it will run further. "
  let ts = showE "sox"
  if isJust ts
    then rawToSoundFile zs nameSF (fromJust ts)
    else printInfoF

-- | Is used to retriev the user-defined file name for the record.
recFileName :: IO String
recFileName = do
  putStrLn "Please, specify the name of the resulting sound file. Please, do NOT use '}' character and space or control characters!"
  nameOfSoundFile <- getLine
  let nameSF = filter (\x -> not (isSpace x) && not (isControl x) && x /= '}') nameOfSoundFile
  return nameSF

getCtrl :: String -> IO String
getCtrl zs = do
  xs <- getLine
  let ys = take (nSymbols . fst . genControl $ zs) xs
  return ys

recFNAndCtrl :: String -> Int -> IO String
recFNAndCtrl zs n
  | odd n = recFileName
  | otherwise = getCtrl zs

nameAndControl :: String -> [Int] -> IO [String]
nameAndControl zs = mapM (recFNAndCtrl zs)

-- | Converts RAW sound to the sound file of the needed format in the current directory accordingly to the 'genControl' for the first 'String' argument.
-- Is used internally in the 'workWithInput'.
rawToSoundFile :: String -> String -> FilePath -> IO ()
rawToSoundFile zs nameSF executablePath
  | null zs = do
     callProcess executablePath ["-r22050","-c1","-L","-esigned-integer","-b16", nameSF ++ ".raw", nameSF ++ ".wav"]
     removeFile $ nameSF ++ ".raw"
  | otherwise = do
     let ws = snd . genControl $ zs
     callProcess executablePath ["-r22050","-c1","-L","-esigned-integer","-b16", nameSF ++ ".raw", fst ws, nameSF ++ snd ws]
     removeFile $ nameSF ++ ".raw"

-- | Prints informational message about ending of the possible for the given data program operation on sound files. Is used internally in the 'workWithInput'.
-- Is used internally in the 'workWithInput'.
printInfoF :: IO ()
printInfoF = do
  putStr "You have a resulting file in a raw PCM format with bitrate 22050 Hz and 1 channel (mono) in the .raw format. "
  putStr "You can further process it by yourself manually, otherwise, please, install FFMpeg or LibAV executables in the directory mentioned in the variable PATH"
  putStrLn " and then run: "
  putStrLn "\"name_of_FFMpeg_or_LibAV_executable\" -f s16le -acodec pcm_s16le -ac 1 -ar 22050 -i \"name_Of_the_sound_file\" \"the_same_name_without_.raw_ending_and_with_.wav_ending\""
  putStrLn ""
  putStrLn "OR you can install SoX executable in the directory mentioned in the variable PATH and then run: "
  putStrLn "\"Path_to_the_SoX_executable\" -b16 -r22050 -c1 -e signed-integer -L \"name_of_the_file_in_raw_format_with_new._prefix\" \"name_of_the_file_in_raw_format_with_new._prefix\" in the terminal."
