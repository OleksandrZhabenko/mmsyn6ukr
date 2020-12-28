-- |
-- Module      :  Melodics.Ukrainian
-- Copyright   :  (c) OleksandrZhabenko 2019-2020
-- License     :  MIT
-- Maintainer  :  olexandr543@yahoo.com
--
-- Functions provide functionality of a musical instrument synthesizer or for Ukrainian speech synthesis
-- especially for poets, translators and writers.
--

module Melodics.Ukrainian (
  appendS16LEFile
  , convertToProperUkrainian
  , takeData
  , isUkrainian
  -- ** Since 0.9.0.0 version -- transformation function.
  , new2OldRepresentation
) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB
import qualified Data.ByteString.Char8 as B
import System.IO
import CaseBi.Unboxed (getBFst')
import qualified CaseBi as X (getBFst')
import qualified Melodics.ByteString.Ukrainian as MU (convertToProperUkrainianS)
import Paths_mmsyn6ukr

{-
-- Inspired by: https://mail.haskell.org/pipermail/beginners/2011-October/008649.html
-}

-- | Function to take raw sound data from the \".wav\" file given.
takeData :: FilePath -> IO B.ByteString
takeData file = do
  data1 <- B.readFile file
  let dataN = B.drop 44 data1 in return dataN

-- | The function that actually produces a .raw file. The mapping table is given in the @Map.txt@ file.
appendS16LEFile ::  VB.Vector String -> Handle -> IO ()
appendS16LEFile xs hdl | not (VB.null xs) =
  do
    dataFileList <- mapM getDataFileName
      ["-.wav", "0.wav", "1.wav", "A.wav", "B.wav", "C.wav", "D.wav", "E.wav", "F.wav", "G.wav", "H.wav",
        "I.wav", "J.wav", "K.wav", "L.wav", "M.wav", "N.wav", "O.wav", "P.wav", "Q.wav", "R.wav",
          "S.wav", "T.wav", "U.wav", "V.wav", "W.wav", "X.wav", "Y.wav", "Z.wav", "a.wav", "b.wav", "c.wav",
            "d.wav", "e.wav", "f.wav"]
    dataList <- VB.mapM takeData . VB.fromList $! dataFileList
    VB.mapM_ (\u ->
      if VB.all (\z -> B.length z > 0) dataList
        then let rs =  tail . dropWhile (/= ' ') . takeWhile (/= '}') . show $ hdl in do
          hClose hdl
          closedHdl <- hIsClosed hdl
          if closedHdl
            then do
                   B.appendFile rs $ dataList VB.! (X.getBFst' (0, VB.fromList [("-", 0), ("0", 1), ("1", 2), ("а", 3), ("б", 4),
                     ("в", 5), ("г", 6), ("д", 7), ("дж", 8), ("дз", 9), ("е", 10), ("ж", 11), ("з", 12), ("и", 13),
                        ("й", 14), ("к", 15), ("л", 16), ("м", 17), ("н", 18), ("о", 19), ("п", 20), ("р", 21),
                          ("с", 22), ("сь", 23), ("т", 24), ("у", 25), ("ф", 26), ("х", 27), ("ц", 28), ("ць", 29), ("ч", 30),
                            ("ш", 31), ("ь", 32), ("і", 33), ("ґ", 34)]) u)
            else error "File is not closed!"
        else error "Data sound file is not read!") xs
    hClose hdl
                       | otherwise = return ()

-- | The function that converts a written Ukrainian text into the sounding in the program phonetical respesentation.
-- It is not exact phonetically but you can make for yourself a general impression of the Ukrainian sounding.
convertToProperUkrainian :: String -> VB.Vector String
convertToProperUkrainian = new2OldRepresentation . MU.convertToProperUkrainianS

new2OldRepresentation :: String -> VB.Vector String
new2OldRepresentation xs = VB.fromList . map f $ xs
  where f = X.getBFst' ("",VB.fromList . zip "-01ABCDEFabcdefghijklmnopqrstuvwxyz" $ ["-","0","1","дз","ж","й","сь","ч","ш","а","б",
              "ц","д","е","ф","ґ","г","і","дж","к","л","м","н","о","п","ь","р","с","т","у","в","ць","х","и","з"])

isUkrainian :: Char -> Bool
isUkrainian y | (y >= '\1040' && y <= '\1065') || (y >= '\1070' && y <= '\1097') = True
              | otherwise = getBFst' (False, V.fromList . map (\x -> (x, True)) $ "'-\700\1028\1030\1031\1068\1100\1102\1103\1108\1110\1111\1168\1169\8217") y
