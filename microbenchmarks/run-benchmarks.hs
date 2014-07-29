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
      (And[ compileParam (show numFuns) ]))
  | numFuns <- [ 2^n | n <- [1..13]] ] 

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
