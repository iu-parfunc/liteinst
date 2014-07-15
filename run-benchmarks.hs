{-# LANGUAGE NamedFieldPuns #-}

-- | HSBencher script to run all the benchmarks.
module Main where
import HSBencher
-- import HSBencher.Backend.Fusion  (defaultFusionPlugin)
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
  [ (mkBenchmark "benchmarks/gzip-1.6/gprof/Makefile" [] (setVariant "gprof")) { progname = Just "gzip16" },
   (mkBenchmark "benchmarks/gzip-1.6/dynaprof/direct_05/Makefile" [] (setVariant "dynaprof_direct_05")) { progname = Just "gzip16" },
--   (mkBenchmark "benchmarks/gzip-1.6/dynaprof/direct_10/Makefile" [] (setVariant "dynaprof_direct_10")) { progname = Just "gzip16" },
--   (mkBenchmark "benchmarks/gzip-1.6/dynaprof/direct_25/Makefile" [] (setVariant "dynaprof_direct_25")) { progname = Just "gzip16" },
   (mkBenchmark "benchmarks/gzip-1.6/dynaprof/direct_50/Makefile" [] (setVariant "dynaprof_direct_50")) { progname = Just "gzip16" },
--   (mkBenchmark "benchmarks/gzip-1.6/pin/Makefile" [] (setVariant "pin")) { progname = Just "gzip16" },
--   (mkBenchmark "benchmarks/gzip-1.6/pebil/Makefile" [] (setVariant "pebil")) { progname = Just "gzip16" },
   (mkBenchmark "benchmarks/gzip-1.6/unprofiled/Makefile" [] (setVariant "unprofiled")) { progname = Just "gzip16" },
-- h264ref-9.3
   (mkBenchmark "benchmarks/h264ref-9.3/unprofiled/Makefile" [] (setVariant "unprofiled")) { progname = Just "h264ref-9.3" },
   (mkBenchmark "benchmarks/h264ref-9.3/dynaprof/Makefile" [] (setVariant "dynaprof_10")) { progname = Just "h264ref-9.3" },
   (mkBenchmark "benchmarks/h264ref-9.3/gprof/Makefile" [] (setVariant "gprof")) { progname = Just "h264ref-9.3" },
-- perl-5.8.7
   (mkBenchmark "benchmarks/perl-5.8.7/unprofiled/Makefile" [] (setVariant "unprofiled")) { progname = Just "perl-5.8.7" },
   (mkBenchmark "benchmarks/perl-5.8.7/dynaprof/Makefile" [] (setVariant "dynaprof_10")) { progname = Just "perl-5.8.7" },
   (mkBenchmark "benchmarks/perl-5.8.7/gprof/Makefile" [] (setVariant "gprof")) { progname = Just "perl-5.8.7" }
-- grep-2.18
--   (mkBenchmark "benchmarks/grep-2.18/gprof/Makefile" [] (setVariant "gprof")) { progname = Just "grep218" },
--   (mkBenchmark "benchmarks/grep-2.18/dynaprof/Makefile" [] (setVariant "dynaprof")) { progname = Just "grep218" },
--   (mkBenchmark "benchmarks/grep-2.18/pin/Makefile" [] (setVariant "pin")) { progname = Just "grep218" },
--   (mkBenchmark "benchmarks/grep-2.18/pebil/Makefile" [] (setVariant "pebil")) { progname = Just "grep218" },
--   (mkBenchmark "benchmarks/grep-2.18/unprofiled/Makefile" [] (setVariant "unprofiled")) { progname = Just "grep218" }

  ]

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
                        `mappend` harvesters conf                        
        }


-- -- | Check for a SELFTIMED line of output.
-- compilerHarvester :: LineHarvester
-- compilerHarvester = taggedLineHarvester "COMPILER" (\ d r -> r{???=d})

