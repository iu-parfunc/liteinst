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
  putStrLn "liteinst: toggle_throughput"

  defaultMainModifyConfig $ \ conf ->
    conf{ benchlist = benches
        , runTimeOut = Just 600
        , plugIns = [-- SomePlugin defaultFusionPlugin,
                     SomePlugin defaultDribblePlugin]
        , harvesters = customTagHarvesterInt    "NUMBER_OF_EXECUTERS"  `mappend`
                       customTagHarvesterDouble "TARGET_TIME" `mappend`
                       customTagHarvesterDouble "ELAPSED_TIME" `mappend`
                      -- customTagHarvesterDouble "MINIMUM_SWITCHES" `mappend`
                      -- customTagHarvesterDouble "MAXIMUM_SWITCHES" `mappend`
                       customTagHarvesterDouble "NUMBER_OF_TOGGLES" `mappend`
                      -- customTagHarvesterInt    "NUMBER_OF_EXECUTERS" `mappend`
                       customTagHarvesterInt    "STRADDLE_POINT" `mappend`
                       --customTagHarvesterDouble "OBSERVED_SWITCHES_TOTAL" `mappend`
                       customTagHarvesterDouble "MINIMUM_FOO_CALLS" `mappend`
                       customTagHarvesterDouble "MAXIMUM_FOO_CALLS" `mappend`
                       --customTagHarvesterDouble "MINIMUM_BAR_CALLS" `mappend`
                       --customTagHarvesterDouble "MAXIMUM_BAR_CALLS" `mappend`
                       customTagHarvesterDouble "TOTAL_FOO_CALLS" `mappend`
                       customTagHarvesterDouble "TOTAL_BAR_CALLS" `mappend`
                       customTagHarvesterDouble "TOTAL_CALLS" `mappend`
                       harvesters conf
        }


benches :: [Benchmark DefaultParamMeaning]
benches =
  -- [mkBenchmark ("toggle-throughput")
  --    [show threads, show duration, show rate]
  --    (Set (Variant "fastinst") (RuntimeEnv "IGNORE_THIS" "0"))
  --    | threads  <- threadss, duration <- durations, rate <- rates]
  [mkBenchmark ("toggle_throughput")
   [show threads, show duration, show rate]
   (And [])
   | threads  <- threadss, duration <- durations, rate <- rates]


-- (Set (Variant "liteinst") (CompileParam "-DONOFF_TOGGLING"))
threadss  = [1..15]
durations = [3.0]
rates     = [0,10,100,1000,10000,100000,500000,1000000,10000000,100000000]
