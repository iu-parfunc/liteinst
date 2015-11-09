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
                       harvesters conf
        }


benches :: [Benchmark DefaultParamMeaning]
benches = [mkBenchmark ("Makefile") [show straddle_pos, show threads, show duration]  (Set (Variant "microbench") (RuntimeEnv "PATCH_WAIT_TIME" "1800")) 
          | threads <- [1..16]
          , straddle_pos <- [0..4] -- zero means not-a-straddler
          , duration <- [0.1,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0]]

