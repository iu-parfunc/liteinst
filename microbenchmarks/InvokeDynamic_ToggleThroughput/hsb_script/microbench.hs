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
        , plugIns = [-- SomePlugin defaultFusionPlugin,
                     SomePlugin defaultDribblePlugin]
        , harvesters = customTagHarvesterInt    "NUM_THREADS"  `mappend`
                       customTagHarvesterDouble "TARGET_TIME" `mappend`
                       customTagHarvesterDouble "ELAPSED_TIME" `mappend`
                       customTagHarvesterDouble "MINIMUM_SWITCHES" `mappend`
                       customTagHarvesterDouble "MAXIMUM_SWITCHES" `mappend`
                       customTagHarvesterDouble "NUMBER_OF_TOGGLES" `mappend`
                       customTagHarvesterInt    "NUMBER_OF_EXECUTERS" `mappend`
                       customTagHarvesterInt    "STRADDLE_POINT" `mappend`
                       customTagHarvesterDouble "OBSERVED_SWITCHES_TOTAL" `mappend`
                       customTagHarvesterDouble "MINIMUM_FOO_CALLS" `mappend`
                       customTagHarvesterDouble "MAXIMUM_FOO_CALLS" `mappend`
                       customTagHarvesterDouble "MINIMUM_BAR_CALLS" `mappend`
                       customTagHarvesterDouble "MAXIMUM_BAR_CALLS" `mappend`
                       customTagHarvesterDouble "TOTAL_FOO_CALLS" `mappend`
                       customTagHarvesterDouble "TOTAL_BAR_CALLS" `mappend`
                       harvesters conf
        }


benches :: [Benchmark DefaultParamMeaning]
benches =
  [mkBenchmark ("toggle-throughput")
   [show threads, show duration, show rate]
   (Set (Variant "java-volatile") (RuntimeEnv "IGNORE_THIS" "0"))
   | threads <- [1..15]
   , duration <- [4.0]
   , rate <- [100,1000,10000,100000,1000000,10000000,100000000]]
