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
  putStrLn "LATENT COST MICROBENCH"

  defaultMainModifyConfig $ \ conf ->
    conf{ benchlist = benches
        , runTimeOut = Just 600
        , plugIns = [SomePlugin defaultFusionPlugin,
                     SomePlugin defaultDribblePlugin]
        , harvesters = customTagHarvesterInt    "LATENT_COST"  `mappend`
                       harvesters conf
        }


benches :: [Benchmark DefaultParamMeaning]
benches = [mkBenchmark ("Makefile") [show straddle_pos, show num_iterations]  (Set (Variant "microbench") (RuntimeEnv "PATCH_WAIT_TIME" "1800"))
          | num_iteations <- [1000000]
          , straddle_pos <- [0..4]] -- zero means not-a-straddler
