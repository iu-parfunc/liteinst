{-# LANGUAGE NamedFieldPuns #-}

-- | HSBencher script to run all the benchmarks.
module Main where
import HSBencher
import HSBencher.Backend.Fusion  (defaultFusionPlugin)
import HSBencher.Backend.Dribble (defaultDribblePlugin)
import HSBencher.Backend.Fusion  (defaultFusionPlugin) 

import Data.Monoid  (mappend)
import qualified Data.Map as M
import System.Environment (getEnvironment)
import System.Directory   (setCurrentDirectory, getDirectoryContents, getCurrentDirectory)
import System.IO.Unsafe   (unsafePerformIO)
import System.Process     
import GHC.Conc           (getNumProcessors)

--------------------------------------------------------------------------------

benches :: [Benchmark DefaultParamMeaning]
benches = 
--  [ shellBenchmark "racket infer-timing.rkt" ("--hsbencher" : words args) (And [])
--  [ (mkBenchmark "benchmarks/gzip-1.6/gprof/Makefile" [] (setVariant "gprof")) { progname = Just "gzip16" },
--   (mkBenchmark "benchmarks/gzip-1.6/dynaprof/direct_05/Makefile" [] (setVariant "dynaprof_direct_05")) { progname = Just "gzip16" },
--   (mkBenchmark "benchmarks/gzip-1.6/dynaprof/direct_10/Makefile" [] (setVariant "dynaprof_direct_10")) { progname = Just "gzip16" },
--   (mkBenchmark "benchmarks/gzip-1.6/dynaprof/direct_25/Makefile" [] (setVariant "dynaprof_direct_25")) { progname = Just "gzip16" },
--   (mkBenchmark "benchmarks/gzip-1.6/dynaprof/direct_50/Makefile" [] (setVariant "dynaprof_direct_50")) { progname = Just "gzip16" },
--   (mkBenchmark "benchmarks/gzip-1.6/pin/Makefile" [] (setVariant "pin")) { progname = Just "gzip16" },
--   (mkBenchmark "benchmarks/gzip-1.6/pebil/Makefile" [] (setVariant "pebil")) { progname = Just "gzip16" },
--   (mkBenchmark "benchmarks/gzip-1.6/unprofiled/Makefile" [] (setVariant "unprofiled")) { progname = Just "gzip16" },

-- h264ref-9.3
   (mkBenchmark "benchmarks/h264ref-9.3/unprofiled/Makefile" [] (setVariant "unprofiled")) { progname = Just "h264ref-9.3" },

   (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/bop_simple_05_10000/Makefile" [] bop_05) { progname = Just "h264ref-9.3" },
   (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/bop_simple_50_10000/Makefile" [] bop_50) { progname = Just "h264ref-9.3" },
   (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/count_only/Makefile" [] count_only) { progname = Just "h264ref-9.3" },
   (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/fixed_backoff_10000/Makefile" [] fixed_backoff_10000) { progname = Just "h264ref-9.3" },
   (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/fixed_backoff_1000000/Makefile" [] fixed_backoff_1000000) { progname = Just "h264ref-9.3" },
   (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/no_backoff/Makefile" [] no_backoff) { progname = Just "h264ref-9.3" },

   
   -- (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/bop_simple_05_10000/Makefile" [] (setVariant "bop_simple_05_10000")) { progname = Just "h264ref-9.3" },
   -- (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/bop_simple_50_10000/Makefile" [] (setVariant "bop_simple_50_10000")) { progname = Just "h264ref-9.3" },
   -- (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/count_only/Makefile" [] (setVariant "count_only")) { progname = Just "h264ref-9.3" },
   -- (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/fixed_backoff_10000/Makefile" [] (setVariant "fixed_backoff_10000")) { progname = Just "h264ref-9.3" },
   -- (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/fixed_backoff_1000000/Makefile" [] (setVariant "fixed_backoff_1000000")) { progname = Just "h264ref-9.3" },
   -- (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/no_backoff/Makefile" [] (setVariant "no_backoff")) { progname = Just "h264ref-9.3" },


-- perl-5.8.7
--   (mkBenchmark "benchmarks/perl-5.8.7/unprofiled/Makefile" [] (setVariant "unprofiled")) { progname = Just "perl-5.8.7" },
--   (mkBenchmark "benchmarks/perl-5.8.7/dynaprof/Makefile" [] (setVariant "dynaprof_10")) { progname = Just "perl-5.8.7" },
--   (mkBenchmark "benchmarks/perl-5.8.7/gprof/Makefile" [] (setVariant "gprof")) { progname = Just "perl-5.8.7" }

-- grep-2.18
--   (mkBenchmark "benchmarks/grep-2.18/gprof/Makefile" [] (setVariant "gprof")) { progname = Just "grep218" },
--   (mkBenchmark "benchmarks/grep-2.18/dynaprof/Makefile" [] (setVariant "dynaprof")) { progname = Just "grep218" },
--   (mkBenchmark "benchmarks/grep-2.18/pin/Makefile" [] (setVariant "pin")) { progname = Just "grep218" },
--   (mkBenchmark "benchmarks/grep-2.18/pebil/Makefile" [] (setVariant "pebil")) { progname = Just "grep218" },
--   (mkBenchmark "benchmarks/grep-2.18/unprofiled/Makefile" [] (setVariant "unprofiled")) { progname = Just "grep218" }

  ]


bop_05 = And [Set (Variant "bop_simple_05_10000") (RuntimeEnv "DYN_STRATEGY" "BOP_SIMPLE")
             ,Set NoMeaning                       (RuntimeEnv "DYN_SAMPLE_SIZE" "10000")
             ,Set NoMeaning                       (RuntimeEnv "DYN_OVERHEAD" "0.05")]
         
bop_50 = And [Set (Variant "bop_simple_50_10000") (RuntimeEnv "DYN_STRATEGY" "BOP_SIMPLE")
             ,Set NoMeaning                       (RuntimeEnv "DYN_SAMPLE_SIZE" "10000")
             ,Set NoMeaning                       (RuntimeEnv "DYN_OVERHEAD" "0.55")]

count_only = Set (Variant "count_only") (RuntimeEnv "DYN_STRATEGY" "COUNT_ONLY")


fixed_backoff_10000 = And [Set (Variant "fixed_backoff_10000") (RuntimeEnv "DYN_STRATEGY" "FIXED_BACKOFF")
                          ,Set NoMeaning                       (RuntimeEnv "DYN_SAMPLE_SIZE" "10000")]

fixed_backoff_1000000 = And [Set (Variant "fixed_backoff_1000000") (RuntimeEnv "DYN_STRATEGY" "FIXED_BACKOFF")
                            ,Set NoMeaning                       (RuntimeEnv "DYN_SAMPLE_SIZE" "1000000")]


no_backoff = Set (Variant "no_backoff") (RuntimeEnv "DYN_STRATEGY" "NO_BACKOFF")


setVariant str = (And [Set (Variant str) (CompileParam "")])

main :: IO ()
main = do
  putStrLn "Begin Dynaprof profiling benchmarks..."
  defaultMainModifyConfig $ \ conf ->
    conf{ benchlist  = benches
        , runTimeOut = Just 1000 -- Erk... need a separate compile timeout.
        , plugIns   = [ SomePlugin defaultFusionPlugin,
                        SomePlugin defaultDribblePlugin ]
        , harvesters = customTagHarvesterDouble "INIT_TIME"
                        `mappend` customTagHarvesterDouble "FINAL_OVERHEAD"
                        `mappend` harvesters conf                        
        }


-- -- | Check for a SELFTIMED line of output.
-- compilerHarvester :: LineHarvester
-- compilerHarvester = taggedLineHarvester "COMPILER" (\ d r -> r{???=d})

