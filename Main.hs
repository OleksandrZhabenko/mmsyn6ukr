-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2019-2020
-- License     :  MIT
-- Maintainer  :  olexandr543@yahoo.com
--
-- A program and a library that can be used as a musical instrument synthesizer or for Ukrainian speech synthesis 
-- especially for poets, translators and writers. 
--

module Main where

import System.Environment (getArgs)
import Melodics.Executable

{-
-- Inspired by: https://mail.haskell.org/pipermail/beginners/2011-October/008649.html
-}

-- | The function creates a raw PCM sound file with bitrate 22050 Hz 1 mono channel 16-bit signed-integer encoding 
-- and tries to automatically convert it to the .wav, .ogg, or .flac file with the same parameters specified by the first command line argument
-- (for more details see: 'genControl' function) using the system binary SoX (this is done for one circle of running, afterwards it is repeated 
-- with the same command line arguments. To stop execution, please, interrupt the program e. g. with Ctrl + C on many Unix platforms). 
-- So actually, it can create multiple sound files, all in the same format options specified by the first command line argument accordingly to 
-- the 'genControl' function.
--
-- If SoX binaries are not installed properly, the program makes ending informational message for the user. 
-- 
-- The command line argument is described in more details in the documentation for the 'Melodics.Ukrainian.nSymbols' function. 
-- 
-- * Notification.
-- 
-- Please, notice that successful usage of the SoX installed in the system at the moment of running the @mmsyn6ukr@ can lead to approximately doubling 
-- (in the most space consuming variant) the size of used space in the storage for the resulting files while being processed because it adds a header 
-- to the .raw file and writes down additionally the raw data to form a .wav file. Afterwards, it deletes the .raw file, so space is used finally in the 
-- more efficient manner. 
-- 
-- Also notice that the size of the largest data file representing a symbol or their combination is 6792 bytes (with 44-byte header included). So, if you 
-- expect to create sounding for @n@ symbols of the Ukrainian text, provide at least @2 * (6792 - 44) * n + 44 = 13496 * n + 44@ (bytes) of the additional 
-- space in the storage (in reality it can occupy much less because other data files are less in size). Afterwards, the program deletes the .raw file (this 
-- will approximately halve the occupied space by the resulting file) and you can manually compress the .wav file (e. g. FLAC compression with the best ratio 
-- gives approximately 0.53 of the original size. Therefore, the resulting file for the @mmsyn6ukr@ executable run prior to such operations without command 
-- line arguments is expected to be less than about @10^7@ bytes that is about 100 MB (for 31416 symbols Ukrainian text)). 
-- 
-- The best comression ratio is with the .ogg files, but they lose some quality so be careful if you need it.
main :: IO ()
main = do
  zs <- fmap (concat . take 1) getArgs
  circle zs
