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
  [ mkBenchmark "sigtrap/" [] noconf ] ++

  [ (mkBenchmark "zcatoggle/initOverhead/" [show numFuns] 
      (And[ compileParam (show numFuns) ])) { progname = Just "firstTimeInitOverheadZcaToggle" }
  | numFuns <- [ 2^n | n <- [1..14]] ] ++

  [ (mkBenchmark "dynaprof/initOverhead/" [show numFuns] 
      (And[ compileParam (show numFuns) ])) { progname = Just "firstTimeInitOverheadDynaProf" }
      -- RRN: nixing 2^14 case.. it crashes.
  | numFuns <- [ 2^n | n <- [1..13]] ] ++ 

  [ (mkBenchmark "dynaprof/deactivationOverhead/" [show numFuns] 
      (And[ compileParam (show numFuns) ])) { progname = Just "deactivationOverheadDynaProf" }
  | numFuns <- [ 2^n | n <- [1..2]] ] ++

  [ (mkBenchmark "dynaprof/sampleOverhead/" [show numFuns, show numCalls, show memTraff] 
      (And[ compileParam (show (2^13)) ])) { progname = Just "sampleOverheadDynaProf" }
      -- RRN: nixing 2^14 case.. it crashes.
  | numFuns  <- [ 2^n  | n <- [1..13]] 
  , numCalls <- [ 10^n | n <- [0..3]]
  , memTraff <- 0 : [ 2^n  | n <- [0, 10, 20, 22, 26]]
  ] 


setVariant str = (And [Set (Variant str) (CompileParam "")])

main :: IO ()
main = do
  putStrLn "Begin Dynaprof profiling benchmarks..."
  defaultMainModifyConfig $ \ conf ->
    conf{ benchlist  = benches
                       -- 1 hour timeout
        , runTimeOut = Just 1000 -- Erk... need a separate compile timeout.
        , plugIns   = [ SomePlugin defaultFusionPlugin,
                        SomePlugin defaultDribblePlugin ]
        , harvesters = customTagHarvesterDouble "INIT_TIME"
                        `mappend` customTagHarvesterDouble "FINAL_OVERHEAD"
                        `mappend` harvesters conf                        
        }


noconf = And []

compileParam str = Set NoMeaning (CompileParam str)
