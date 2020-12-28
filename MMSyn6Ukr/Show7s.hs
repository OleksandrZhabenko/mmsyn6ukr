-- |
-- Module      :  MMSyn6Ukr.Show7s
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Can be used to show a sorted list of the Ukrainian sounds 
-- representations that for mmsyn7 series of programs. Is taken from 
-- the mmsyn7s package.
--

module MMSyn6Ukr.Show7s (
  show7s
) where

import qualified Data.Vector as V
import Data.List (sort, nub)
import Melodics.Ukrainian (convertToProperUkrainian)

-- | Function takes a Ukrainian text being a @String@ and returns a sorted list of the Ukrainian sounds representations that can be used further in mmsyn7 series of
-- programs.
show7s :: String -> [String]
show7s = sort . nub . V.toList . V.filter (\x -> x /= "-" && x /= "1" && x /= "0") . convertToProperUkrainian
