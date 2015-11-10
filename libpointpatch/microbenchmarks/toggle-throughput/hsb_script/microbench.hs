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
  putStrLn "PATCH TOGGLE THROUGHPUT  MICROBENCH"

  defaultMainModifyConfig $ \ conf ->
    conf{ benchlist = benches
        , runTimeOut = Just 600
        , plugIns = [SomePlugin defaultFusionPlugin,
                     SomePlugin defaultDribblePlugin]
        , harvesters = customTagHarvesterInt    "NUM_THREADS"  `mappend`
                       customTagHarvesterDouble "TARGET_TIME" `mappend`
                       customTagHarvesterDouble "ELLAPSED_TIME" `mappend`
                       customTagHarvesterInt    "MINIMUM_SWITCHES" `mappend`
                       customTagHarvesterInt    "MAXIMUM_SWITCHES" `mappend`
                       customTagHarvesterInt    "NUMBER_OF_TOGGLES" `mappend`
                       customTagHarvesterInt    "NUMBER_OF_EXECUTERS" `mappend`
                       customTagHarvesterInt    "STRADDLE_POINT" `mappend`
                       customTagHarvesterInt    "OBSERVED_SWITCHES_TOTAL" `mappend`
                       customTagHarvesterInt    "MINIMUM_FOO_CALLS" `mappend`
                       customTagHarvesterInt    "MAXIMUM_FOO_CALLS" `mappend`
                       customTagHarvesterInt    "MINIMUM_BAR_CALLS" `mappend`
                       customTagHarvesterInt    "MAXIMUM_BAR_CALLS" `mappend`
                       customTagHarvesterInt    "TOTAL_FOO_CALLS" `mappend`
                       customTagHarvesterInt    "TOTAL_BAR_CALLS" `mappend`
                       harvesters conf
        }


benches :: [Benchmark DefaultParamMeaning]
benches =
  [mkBenchmark ("Makefile")
   [show straddle_pos, show threads, show duration, show rate]
   (And [ Set (Variant "microbench")
              (RuntimeEnv "PATCH_WAIT_TIME" "1800")
        , Set NoMeaning (CompileParam "-O2")])
          | threads <- [1..15]
          , straddle_pos <- [0..4] -- zero means not-a-straddler
          , duration <- [0.1,1.0,2.0,3.0,4.0,5.0]
          , rate <- [250000, 500000, 750000, 1000000, 1250000, 150000 ]]

