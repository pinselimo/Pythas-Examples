module Wonderland where

import System.Directory (doesFileExist, removeFile)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (elemIndex)

import Wonderland.IFace
import Wonderland.Types (horrorParams, state1990, dreamParams, dream1992, horror1992
             , horrorWithAbatement, horrorWithTaxes, getVar
             , WonderState(..), WonderParameters(..))

runWonderland :: String -> Int -> [(Double, Double, Double, Double)]
runWonderland p_id count = let
    params = case p_id of
        "horror" -> horror1992
        "dream"  -> dream1992
        "abate"  -> horrorWithAbatement
        "taxes"  -> horrorWithTaxes 0.1
        _        -> horrorParams
    in map toTuple $ listResult params state1990 count
    where toTuple :: WonderState -> (Double, Double, Double, Double)
          toTuple st = (
                  getVar $ perCapitaOutput st
                , getVar $ naturalCapitalStock st
                , getVar $ population st
                , getVar $ pollutionPerUnitOutput st
                )
