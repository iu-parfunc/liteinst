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
{-
new setup

Sampling profiler: 
 two configuration parameters
  SAMPLE_SIZE
  EPOCH_PERIOD

Backoff profiler
  SAMPLE_SIZE

to decide which profiler
 PROFILER_TYPE=
"FIXED_BACKOFF | SAMPLING"


EPOCH_PERIOD = "1,10,100,1000"
SAMPLE_SIZE="100,1000,10000,100000"
-} 

main :: IO ()
main = do
  putStrLn "Begin Ubiprof profiling benchmarks..."
  defaultMainModifyConfig $ \ conf ->
    conf{ benchlist  = benches
                       -- long timeout 
        , runTimeOut = Just 600 -- Erk... need a separate compile timeout.
        , plugIns   = [ SomePlugin defaultFusionPlugin,
                        SomePlugin defaultDribblePlugin ]
         , harvesters = customTagHarvesterInt   "NUM_PROBES"              `mappend` 
                       customTagHarvesterInt    "CALLED_FUNCTIONS"        `mappend`
                       customTagHarvesterDouble "MAIN"                    `mappend`
                       customTagHarvesterDouble "MONITOR"                 `mappend`
                       customTagHarvesterDouble "DELTA"                   `mappend`
                       customTagHarvesterDouble "PROBE_OVERHEAD"          `mappend`
                       customTagHarvesterDouble "JUMP_OVERHEAD"           `mappend`
                       customTagHarvesterDouble "CUMULATIVE_OVERHEAD"     `mappend`
                       customTagHarvesterDouble "REAL_EXEC_TIME"          `mappend`
                       customTagHarvesterDouble "EXEC_TIME"               `mappend`
                       customTagHarvesterInt    "PROBE_COUNT"             `mappend`
                       harvesters conf             
       

        }

epochs :: [Int] 
epochs = [100,250,500,750,1000,2500,5000]

samplesize :: [Int] 
samplesize = [100,1000,10000,100000]

benchmark_names = ["blackscholes", "hull", "nbody","bzip-1.0.3", "perl-5.8.7", "hmmer", "sjeng", "lbm"]

benches :: [Benchmark DefaultParamMeaning]
benches =
  [ (mkBenchmark ("benchmarks/"++name++"/"++var_name++"/Makefile") [] variant)
    { progname = Just name } 
    | name <- benchmark_names
    , (var_name, variant) <- base_variants] ++

  [ (mkBenchmark ("benchmarks/"++name++"/"++var_name++"/Makefile") [] variant)
    { progname = Just name }
    | name <- benchmark_names
    , (var_name, variant) <- ubiprof_variants] 

  where
    base  = ["gprof", "unprofiled" ]
    base_variants = [(nom, setVariant nom) | nom <- base]
    
    setVariant str = And [Set (Variant str) (CompileParam "")
                          , Set NoMeaning (CompileEnv "CC" "gcc")
                          , Set NoMeaning (CompileEnv "OPTLEVEL" "-O2")  
                          ]


    ubiprof_variants = [ ("ubiprof", backoff)]
                       --Or [ sampling 
                       --   , backoff  
                       --   ]) ]
    

-- sampling = Or [And [ Set (Variant (compiler ++ "_" ++ optlevel ++ "_Sampling_"++show s_size++"_"++show epoch)) (RuntimeEnv "PROFILER_TYPE" "SAMPLING")
--                    , Set NoMeaning (RuntimeEnv "SAMPLE_SIZE" (show s_size))
--                    , Set NoMeaning (RuntimeEnv "EPOCH_PERIOD" (show epoch))
--                    , Set NoMeaning (CompileEnv "CC"  compiler)
--                    , Set NoMeaning (CompileEnv "OPTLEVEL"  optlevel) 
--                    ]
--               | s_size <- samplesize
--               , epoch <- epochs
--               , compiler <- ["gcc", "icc"]
--               , optlevel <- ["-O1", "-O2", "-O3" ]
--               ]



backoff_params = [x * 10^6 | x <- [1..10]] 
backoff  = Or [And [ Set (Variant (compiler ++ "_" ++ optlevel ++ "_Backoff_"++show s_size)) (RuntimeEnv "PROFILER_TYPE" "FIXED_BACKOFF")
                   , Set NoMeaning (RuntimeEnv "SAMPLE_SIZE" (show s_size))
                   , Set NoMeaning (CompileEnv "CC" compiler)
                   , Set NoMeaning (CompileEnv "OPTLEVEL" optlevel)  
                   ]
              | s_size <- backoff_params -- samplesize
              , compiler <- ["gcc"]
              , optlevel <- ["-O2"]
              ] 




-- backoff  = Or [And [ Set (Variant (compiler ++ "_" ++ optlevel ++ "_Backoff_"++show s_size)) (RuntimeEnv "PROFILER_TYPE" "FIXED_BACKOFF")
--                    , Set NoMeaning (RuntimeEnv "SAMPLE_SIZE" (show s_size))
--                    , Set NoMeaning (CompileEnv "CC" compiler)
--                    , Set NoMeaning (CompileEnv "OPTLEVEL" optlevel)  
--                    ]
--               | s_size <- samplesize
--               , compiler <- ["gcc", "icc"]
--               , optlevel <- ["-O1", "-O2", "-O3" ]
--               ] 











---------------------------------------------------------------------------
-- Old stuff
---------------------------------------------------------------------------
-- main :: IO ()
-- main = do
--   putStrLn "Begin Dynaprof profiling benchmarks..."
--   defaultMainModifyConfig $ \ conf ->
--     conf{ benchlist  = benches
--                        -- 1 hour timeout
--         , runTimeOut = Just 1000 -- Erk... need a separate compile timeout.
--         , plugIns   = [ SomePlugin defaultFusionPlugin,
--                         SomePlugin defaultDribblePlugin ]
--         , harvesters = customTagHarvesterDouble "INIT_OVERHEAD_ZCATOGGLE" `mappend` 
--                        customTagHarvesterDouble "INIT_OVERHEAD_DYNAPROF"  `mappend` 
--                        customTagHarvesterDouble "DYN_TEARDOWN"            `mappend` 
--                        customTagHarvesterInt    "NUM_SAMPLES"             `mappend` 
--                        customTagHarvesterInt    "NUM_PROBES"              `mappend` 
--                        customTagHarvesterInt    "NUM_ISLANDS"             `mappend` 
--                        customTagHarvesterInt    "NUM_EPOCHS"              `mappend` 
--                        customTagHarvesterInt    "MMAP_RETRIES"            `mappend` 
--                        customTagHarvesterInt    "TOTAL_THREADS"           `mappend` 
--                        customTagHarvesterInt    "CALLED_FUNCTIONS"        `mappend` 
--                        customTagHarvesterDouble "FINAL_OVERHEAD"   `mappend`
--                        harvesters conf
--         }







-- fixed_backoffLevels :: [Integer]
-- -- fixed_backoffLevels = [ 10^i | i <- [0..6] ]
-- --fixed_backoffLevels = [ 1, 10, 100 ]
-- fixed_backoffLevels = [1,5,10,50,100,500,1000,5000,10000,50000,100000,500000,1000000,5000000,10000000]

-- -- fixed_backoffLevels = [ round (10 ** i) | i <- [0, 0.5 .. 6.5] ]

-- -- [2014.08.07] Going bigger:
-- -- fixed_backoffLevels = [ 10^7, 10^8 ]

-- benches :: [Benchmark DefaultParamMeaning]
-- benches = 
--   [ (mkBenchmark ("benchmarks/"++name++"/"++varname++"/Makefile") [] variant) 
--      { progname = Just name }
--     -- All benchmarks support the basic variants, but grep/gzip also support pebil/pin:
--     | (name,coreVariants) <- [ ("gzip-1.6",    moreVariants)
--                              , ("grep-2.18",   moreVariants)
--                              , ("h264ref-9.3", baseVariants)
--                              , ("bzip-1.0.3",  baseVariants)
--                              , ("perl-5.8.7",  baseVariants)
--                              , ("raxml",       baseVariants)
--                              ]
--     , (varname,variant) <- [ (v, setVariant v) | v <- coreVariants ] ++
--                            [ ("dynaprof", Or [ -- empty_strat, 
--                                                resampling
--                                              , fixed_backoff
--                                              , no_backoff 
--                                              ]) ]
--   ]
--  where baseVariants = ["gprof", "unprofiled" ]
--        moreVariants = baseVariants ++ ["pebil", "pin", "oprofile"]


-- fixed_backoff = Or [ And [ Set (Variant ("fixed_backoff_"++show num)) 
--                                         (RuntimeEnv "DYN_STRATEGY" "FIXED_BACKOFF")
--                          , Set NoMeaning (RuntimeEnv "DYN_SAMPLE_SIZE" (show num)) 
--                          , Set NoMeaning (RuntimeEnv "DYN_DISABLE_SELFTIMED" "1")
--                          , Set NoMeaning (RuntimeEnv "DYN_OUTPUT_TYPE" "NONE")
--                          ]
--                    | num <- fixed_backoffLevels ]

-- empty_strat = Set (Variant ("empty_strat"))
--                   (RuntimeEnv "DYN_STRATEGY" "EMPTY")

-- -- | This version varies both the backoff count and the epoch length
-- --   (time before global reenable of probes).
-- resampling = Or [ And [ Set (Variant ("resampling_"++show num++"_"++show period)) 
--                                         (RuntimeEnv "DYN_STRATEGY" "SAMPLING")
--                       , Set NoMeaning (RuntimeEnv "DYN_SAMPLE_SIZE" (show num)) 
--                       , Set NoMeaning (RuntimeEnv "DYN_SAMPLE_PERIOD" (show period))
--                       , Set NoMeaning (RuntimeEnv "DYN_DISABLE_SELFTIMED" "1")
--                       , Set NoMeaning (RuntimeEnv "DYN_OUTPUT_TYPE" "NONE")
--                       ]
-- --                   | num    <- [ 10^i | i <- [0..5] ] -- Hold back a little more
--                    | num <- [10^i | i <- [3..8]] 
-- --                   | num    <- [ 10^6, 10^7, 10^8 ]

-- --                   , period <- [ 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1.0, 2.0 ] 
--                    , period <- [ 0.05, 0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0] ]

-- no_backoff = Set (Variant "no_backoff") (RuntimeEnv "DYN_STRATEGY" "NO_BACKOFF")

-- ----------------------------------------
-- -- OLD, SCRAP:
-- ----------------------------------------

-- bop_05 = And [Set (Variant "bop_simple_05_10000") (RuntimeEnv "DYN_STRATEGY" "BOP_SIMPLE")
--              ,Set NoMeaning                       (RuntimeEnv "DYN_SAMPLE_SIZE" "10000")
--              ,Set NoMeaning                       (RuntimeEnv "DYN_OVERHEAD" "0.05")]
         
-- bop_50 = And [Set (Variant "bop_simple_50_10000") (RuntimeEnv "DYN_STRATEGY" "BOP_SIMPLE")
--              ,Set NoMeaning                       (RuntimeEnv "DYN_SAMPLE_SIZE" "10000")
--              ,Set NoMeaning                       (RuntimeEnv "DYN_OVERHEAD" "0.50")] --should be 50 right ?? 

-- count_only = Set (Variant "count_only") (RuntimeEnv "DYN_STRATEGY" "COUNT_ONLY")

-- fixed_backoff_10000 = And [Set (Variant "fixed_backoff_10000") (RuntimeEnv "DYN_STRATEGY" "FIXED_BACKOFF")
--                           ,Set NoMeaning                       (RuntimeEnv "DYN_SAMPLE_SIZE" "10000")]

-- fixed_backoff_1000000 = And [Set (Variant "fixed_backoff_1000000") (RuntimeEnv "DYN_STRATEGY" "FIXED_BACKOFF")
--                             ,Set NoMeaning                       (RuntimeEnv "DYN_SAMPLE_SIZE" "1000000")]



-- -- varied "dynaprof" sample_rate sample_size
-- --varied sr ss = And [ Set (Variant ("Varied_" ++ show sr ++ "_" ++ show ss)) (RunTimeEnv "DYN_STRATEGY" "SAMPLING")
-- --                   , Set NoMeaning                                          (RunTimeEnv "DYN_SAMPLE_SIZE" (show ss))
-- --                   , Set NoMeaning                                          (RunTimeEnv "DYN_SAMPLING_RATE" (show sr))] 


-- setVariant str = (And [Set (Variant str) (CompileParam "")])


-- -- -- | Check for a SELFTIMED line of output.
-- -- compilerHarvester :: LineHarvester
-- -- compilerHarvester = taggedLineHarvester "COMPILER" (\ d r -> r{???=d})

