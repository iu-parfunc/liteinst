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

backoffLevels :: [Integer]
backoffLevels = [ 10^i | i <- [0..6] ]

benches :: [Benchmark DefaultParamMeaning]
benches = 
  [ (mkBenchmark ("benchmarks/gzip-1.6/"++varname++"/Makefile") [] variant) { progname = Just "gzip16" }
  | (varname,variant) <- [ (v, setVariant v) | v <- ["gprof", "unprofiled", "pebil", "pin"]] ++
                         [ ("dynaprof", fixed_backoff n) | n <- backoffLevels ]
  ] ++ 
   [
-- h264ref-9.3
   (mkBenchmark "benchmarks/h264ref-9.3/unprofiled/Makefile" [] (setVariant "unprofiled")) { progname = Just "h264ref-9.3" },
   (mkBenchmark "benchmarks/h264ref-9.3/unprofiled-instrumented/Makefile" [] (setVariant "unprofiled-inst")) { progname = Just "h264ref-9.3" },
   (mkBenchmark "benchmarks/h264ref-9.3/gprof/Makefile" [] (setVariant "gprof")) { progname = Just "h264ref-9.3" },

   (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/Makefile" [] bop_05) { progname = Just "h264ref-9.3" },
   (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/Makefile" [] bop_50) { progname = Just "h264ref-9.3" },
 --  (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/count_only/Makefile" [] count_only) { progname = Just "h264ref-9.3" },
   (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/Makefile" [] fixed_backoff_10000) { progname = Just "h264ref-9.3" },
   (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/Makefile" [] fixed_backoff_1000000) { progname = Just "h264ref-9.3" },
   (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/Makefile" [] no_backoff) { progname = Just "h264ref-9.3" },

   --(mkBenchmark "benchmarks/h264ref-9.3/dynaprof/fixed_backoff_10000/Makefile" [] fixed_backoff_10000) { progname = Just "h264ref-9.3" },
   --(mkBenchmark "benchmarks/h264ref-9.3/dynaprof/fixed_backoff_1000000/Makefile" [] fixed_backoff_1000000) { progname = Just "h264ref-9.3" },
   --(mkBenchmark "benchmarks/h264ref-9.3/dynaprof/no_backoff/Makefile" [] no_backoff) { progname = Just "h264ref-9.3" },

-- bzip
   (mkBenchmark "benchmarks/bzip-1.0.3/unprofiled/Makefile" [] (setVariant "unprofiled")) { progname = Just "bzip-1.0.3" },
   (mkBenchmark "benchmarks/bzip-1.0.3/unprofiled-instrumented/Makefile" [] (setVariant "unprofiled-inst")) { progname = Just "bzip-1.0.3" },
   (mkBenchmark "benchmarks/bzip-1.0.3/gprof/Makefile" [] (setVariant "gprof")) { progname = Just "bzip-1.0.3" },

   (mkBenchmark "benchmarks/bzip-1.0.3/dynaprof/Makefile" [] bop_05) { progname = Just "bzip-1.0.3" },
   (mkBenchmark "benchmarks/bzip-1.0.3/dynaprof/Makefile" [] bop_50) { progname = Just "bzip-1.0.3" },
   (mkBenchmark "benchmarks/bzip-1.0.3/dynaprof/Makefile" [] fixed_backoff_10000) { progname = Just "bzip-1.0.3" },
   (mkBenchmark "benchmarks/bzip-1.0.3/dynaprof/Makefile" [] fixed_backoff_1000000) { progname = Just "bzip-1.0.3" },
   (mkBenchmark "benchmarks/bzip-1.0.3/dynaprof/Makefile" [] no_backoff) { progname = Just "bzip-1.0.3" },
  

   --(mkBenchmark "benchmarks/bzip-1.0.3/dynaprof/fixed_backoff_10000/Makefile" [] fixed_backoff_10000) { progname = Just "bzip-1.0.3" },
   --(mkBenchmark "benchmarks/bzip-1.0.3/dynaprof/fixed_backoff_1000000/Makefile" [] fixed_backoff_1000000) { progname = Just "bzip-1.0.3" },
   --(mkBenchmark "benchmarks/bzip-1.0.3/dynaprof/no_backoff/Makefile" [] no_backoff) { progname = Just "bzip-1.0.3" },
   

   
   -- (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/bop_simple_05_10000/Makefile" [] (setVariant "bop_simple_05_10000")) { progname = Just "h264ref-9.3" },
   -- (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/bop_simple_50_10000/Makefile" [] (setVariant "bop_simple_50_10000")) { progname = Just "h264ref-9.3" },
   -- (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/count_only/Makefile" [] (setVariant "count_only")) { progname = Just "h264ref-9.3" },
   -- (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/fixed_backoff_10000/Makefile" [] (setVariant "fixed_backoff_10000")) { progname = Just "h264ref-9.3" },
   -- (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/fixed_backoff_1000000/Makefile" [] (setVariant "fixed_backoff_1000000")) { progname = Just "h264ref-9.3" },
   -- (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/no_backoff/Makefile" [] (setVariant "no_backoff")) { progname = Just "h264ref-9.3" },

   (mkBenchmark "benchmarks/perl-5.8.7/unprofiled/Makefile" [] (setVariant "unprofiled")) { progname = Just "perl-5.8.7" },
   (mkBenchmark "benchmarks/perl-5.8.7/dynaprof/Makefile" [] (setVariant "dynaprof_10")) { progname = Just "perl-5.8.7" },
   (mkBenchmark "benchmarks/perl-5.8.7/gprof/Makefile" [] (setVariant "gprof")) { progname = Just "perl-5.8.7" },

   (mkBenchmark "benchmarks/grep-2.18/gprof/Makefile" [] (setVariant "gprof")) { progname = Just "grep218" },
   (mkBenchmark "benchmarks/grep-2.18/dynaprof/Makefile" [] (setVariant "dynaprof")) { progname = Just "grep218" },
   (mkBenchmark "benchmarks/grep-2.18/pin/Makefile" [] (setVariant "pin")) { progname = Just "grep218" },
   (mkBenchmark "benchmarks/grep-2.18/pebil/Makefile" [] (setVariant "pebil")) { progname = Just "grep218" },
   (mkBenchmark "benchmarks/grep-2.18/unprofiled/Makefile" [] (setVariant "unprofiled")) { progname = Just "grep218" },

   (mkBenchmark "benchmarks/raxml/unprofiled/Makefile" [] (setVariant "unprofiled")) { progname = Just "RAxML" },
   (mkBenchmark "benchmarks/raxml/dynaprof/Makefile"   [] (setVariant "dynaprof"))   { progname = Just "RAxML" }

  ]


bop_05 = And [Set (Variant "bop_simple_05_10000") (RuntimeEnv "DYN_STRATEGY" "BOP_SIMPLE")
             ,Set NoMeaning                       (RuntimeEnv "DYN_SAMPLE_SIZE" "10000")
             ,Set NoMeaning                       (RuntimeEnv "DYN_OVERHEAD" "0.05")]
         
bop_50 = And [Set (Variant "bop_simple_50_10000") (RuntimeEnv "DYN_STRATEGY" "BOP_SIMPLE")
             ,Set NoMeaning                       (RuntimeEnv "DYN_SAMPLE_SIZE" "10000")
             ,Set NoMeaning                       (RuntimeEnv "DYN_OVERHEAD" "0.50")] --should be 50 right ?? 

count_only = Set (Variant "count_only") (RuntimeEnv "DYN_STRATEGY" "COUNT_ONLY")


fixed_backoff num = And [ Set (Variant ("fixed_backoff_"++show num)) 
                                        (RuntimeEnv "DYN_STRATEGY" "FIXED_BACKOFF")
                        , Set NoMeaning (RuntimeEnv "DYN_SAMPLE_SIZE" (show num)) ]


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

