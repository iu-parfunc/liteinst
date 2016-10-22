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

------------------------------------------------------------
--- BENCHMARKS
------------------------------------------------------------

benches :: [Benchmark DefaultParamMeaning]
benches =
  [ (mkBenchmark ("./"++variant_name++"/Makefile") [] variant) 
       {progname = Just "injection_costs" }
  | (variant_name,variant) <- variants]


  where
    variants = [(v, setVariant v) | v <- ["dyninst","liteinst"]]

    setVariant v = And [Set (Variant v) (RuntimeEnv "DUMMY" "nada")]
    
------------------------------------------------------------
-- Harvesters 
------------------------------------------------------------
harv = customTagHarvesterInt "PROBING_COST" `mappend`
       customTagHarvesterInt "INSERTION_COST" `mappend`
       customTagHarvesterInt "METADATA_COST" `mappend`
       customTagHarvesterInt "PUNNING_COST" `mappend`
       customTagHarvesterInt "COST_RESUMING" `mappend`
       customTagHarvesterInt "ATTACH_COST"  
       
       
------------------------------------------------------------
-- MAIN
------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "Liteinst: Injection_costs microbenchmark"


  defaultMainModifyConfig $ \conf ->
    conf { benchlist = benches
         , runTimeOut = Just 600
         , plugIns = [SomePlugin defaultDribblePlugin]
         , harvesters = harv `mappend` harvesters conf
         } 

  
