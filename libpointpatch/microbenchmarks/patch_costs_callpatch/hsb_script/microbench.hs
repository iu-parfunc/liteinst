{-# LANGUAGE NamedFieldPuns #-}

-- | HSBencher script to run the microbench

module Main where

import HSBencher
import HSBencher.Backend.Fusion (defaultFusionPlugin)
import HSBencher.Backend.Dribble (defaultDribblePlugin)
import HSBencher.Backend.Fusion (defaultFusionPlugin)
import Data.Monoid (mappend)
import qualified Data.Map as M
import System.Environment (getEnvironment)
import System.Directory (setCurrentDirectory, getDirectoryContents, getCurrentDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import GHC.Conc (getNumProcessors)


main :: IO ()
main = do
  putStrLn "PATCHING COST MICROBENCH"

  defaultMainModifyConfig $ \ conf ->
    conf{ benchlist = benches
        , runTimeOut = Just 600
        , plugIns = [SomePlugin defaultFusionPlugin,
                     SomePlugin defaultDribblePlugin]
        , harvesters = customTagHarvesterInt    "NUM_THREADS"  `mappend`
                       customTagHarvesterDouble "ACTIVATION_COST_AVG" `mappend`
                       customTagHarvesterDouble "DEACTIVATION_COST_AVG" `mappend`
                       harvesters conf
        }


benches :: [Benchmark DefaultParamMeaning]
benches = [mkBenchmark ("Makefile") [show threads,show straddle_pos]  (Set (Variant "microbench") (RuntimeEnv "PATCH_WAIT_TIME" "1800"))
          | threads <- [0,1,2,4,8,12,15]
          , straddle_pos <- [0..4]] -- zero means not-a-straddler
