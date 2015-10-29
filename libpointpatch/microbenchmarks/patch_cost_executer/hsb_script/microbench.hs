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
  putStrLn "PATCHING COST IN EXECUTER MICROBENCH"

  defaultMainModifyConfig $ \ conf ->
    conf{ benchlist = benches
        , runTimeOut = Just 600
        , plugIns = [SomePlugin defaultFusionPlugin,
                     SomePlugin defaultDribblePlugin]
        , harvesters = customTagHarvesterInt    "NUM_THREADS"  `mappend`
                       customTagHarvesterInt    "STRADDLE_POINT" `mappend`
                       customTagHarvesterInt    "NUM_ACTIVATIONS" `mappend`
                       harvesters conf
        }


benches :: [Benchmark DefaultParamMeaning]
benches = [mkBenchmark ("Makefile") [show threads,show straddle_pos, show (num_activations*1000)]  (Set (Variant "microbench") (RuntimeEnv "PATCH_WAIT_TIME" "1800")) 
          | threads <- [1..32]
          , straddle_pos <- [0..4] -- zero means not-a-straddler
          , num_activations <- [0..100]]

