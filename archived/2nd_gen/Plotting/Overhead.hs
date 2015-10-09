

import System.Environment (getArgs)

import HSBencher.Analytics


import Data.List hiding (lines)
import Data.Maybe
import Prelude hiding (log, lines)

-- Temporarily
import Network.Google.FusionTables 
-- import Network.Google (FTString)


{- Generate Overhead plots! -} 


cid :: String
cid  = "925399326325-6dir7re3ik7686p6v3kkfkf1kj0ec7ck.apps.googleusercontent.com" 

csec :: String
csec = "MQ72ZWDde_1e1ihI5YE9YlEi"  

table_name = "Dynaprof_Benchmarks" 


-- Maximum number of datapoints to use 
-- num_samples = 30 

-- What benchmark 
benchmark = "h264ref-9.3" 

-- what computer
machine = "hive" -- "tank"

variants = [ "bop_simple_05_10000"
           , "bop_simple_50_10000" ]


main :: IO ()
main = do

  --tab <- pullEntireTable cid csec table_name
  tab <- pullSelectively cid csec table_name "HOSTNAME" machine 
  
  putStrLn "Showing all recieved data"
  putStrLn "------------------------------------------------------------"
  putStrLn $ show tab
  putStrLn "------------------------------------------------------------"

  let (ColData cols values) = tab 

  -- Collect all data of a given benchmark on a given machine. 
  let all_data = slice "PROGNAME" benchmark cols values

  let git_depths = map convertInt $ extractColumn "GIT_DEPTH" cols all_data
      
      run_times = map (extractVariantMedianTime cols all_data) variants

      overheads = map (extractVariantOverhead cols all_data) variants 

      combo = map sortIt $ map (zip git_depths) (map (map convert) run_times)
  
  
  
  -- average out runtimes on the same git_depth
  let avg_combo = map average_at_git_depth combo 
  

  -- For actual overhead, compare to runtime of unprofiled code.
  let unprofiled_data = slice "PROGNAME" "unprofiled" cols all_data
      
      -- get a recent number for unprofiled
      unprofiled_run_times = extractVariantMedianTime cols all_data "unprofiled"
      gr = zip git_depths unprofiled_run_times
      sgr = filter (not . nullFT . snd)
            $ sortIt gr -- reverse $ sortBy (\(g,_) (g',_) -> compare g g') gr
      unprofiled_d = map (\(g,r) -> (g,convert r)) sgr
      unprofiled_avg = average_at_git_depth unprofiled_d
      -- TODO check errors 
      base = snd $ head unprofiled_avg
      

  -- Calc overhead percentage. 
  let overhead (g,x) =(g, (x - base) / base) -- ((x - base) / x)

      real_overheads = map (map overhead) avg_combo  
      
  
  putStrLn "Showing values"
  putStrLn "--RUNTIMES----------------------------------------------------------"
  putStrLn $ show run_times 
  putStrLn "--AVG COMBO----------------------------------------------------------"
  putStrLn $ show avg_combo
  putStrLn "--OVERHEADS----------------------------------------------------------"
  putStrLn $ show overheads
  putStrLn "--UNPROFILED AVERAGE----------------------------------------------------------"
  putStrLn $ show unprofiled_avg
  putStrLn "--REAL OVERHEADS ----------------------------------------------------------"
  putStrLn $ show real_overheads
  putStrLn "------------------------------------------------------------"


-- START THE PLOTING 


  let dyna05_graph = LineGraph "#F00"
                     "dyna05 overhead"
                     Nothing
                     (real_overheads !! 0) -- first data series

  let dyna50_graph = LineGraph "#0FF"
                     "dyna50 overhead"
                     Nothing
                     (real_overheads !! 1) -- second data series 
  

  let plot = Plot {pLines = [dyna05_graph,dyna50_graph],
                   pPoints = [],
                   pBars = [],
                   pLegend = True,
                   pDimensions = (800,400),
                   pXLabel = "GIT DEPTH",
                   pYLabel = "Real overhead %",
                   pXAxisTicks = Nothing,
                   pYAxisTicks = Nothing,
                   pXAxisLog = False,
                   pYAxisLog = False} 
                            
                                
                           
  putStrLn $ html $ renderPlot mySupply plot     


--------------------------------------------------------------------------
-- extract data given a variant

extractVariantMedianTime :: [String] -> [[FTValue]] -> String -> [FTValue]
extractVariantMedianTime cols dat variant =
  let rows = slice "VARIANT" variant cols dat
  in  extractColumn "MEDIANTIME" cols rows 


-- extract the estimated overhead.
extractVariantOverhead :: [String] -> [[FTValue]] -> String -> [FTValue]
extractVariantOverhead cols dat variant =
  let rows = slice "VARIANT" variant cols dat
  in  extractColumn "FINAL_OVERHEAD" cols rows 


---------------------------------------------------------------------------
-- Utilities

nullFT (StringValue []) = True
nullFT _ = False


average_at_git_depth :: [(Int,Double)] -> [(Int,Double)]
average_at_git_depth xs  = result 
  
  where groups = groupBy (\(g,_) (g',_) -> g == g') xs 
        nubbed = nub $ concat $ map (map fst) groups
        avgs   = map average (map (map snd) groups)
        result = zip nubbed avgs 


sortIt r = reverse $ sortBy (\(g,_) (g',_) -> compare g g') r

--------------------------------------------------------------------------
-- Converting and Slicing.
-- This should be improved and placed in the library. 


-- convert is cheating a bit. 
convert :: FTValue -> Double
convert (DoubleValue v) = v
convert (StringValue s) = read s 

convertInt :: FTValue -> Int
convertInt (DoubleValue v) = truncate v
convertInt (StringValue s) = truncate $ (read s :: Double)

extractColumn str header tab =
  [x !! ix | x <- tab] 
  where
    ix  = fromJust $ elemIndex str header 
   


slice col value header values =
  [x | x <- values , x !! ix == (StringValue value)] 
  where
    ix = fromJust $ elemIndex col header 


---------------------------------------------------------------------------
-- Compute average of a list of positive double
-- Negative result means there is an error (probably missing data in fusiontable) 
average :: [Double] -> Double
average [] = -1
average xs = sum xs / (fromIntegral (length xs))
