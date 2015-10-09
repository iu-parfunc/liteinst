

{- Plot the first time init overhead
   That is the firstTimeInitOverheadZcaToggle and
               firstTimeInitOverheadDynaProf  programs

    - Will plot the mediantime field of these benchmarks

    ARGS: 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384 
-}



import System.Environment (getArgs)

import HSBencher.Analytics


import Data.List hiding (lines)
import Data.Maybe
import Prelude hiding (log, lines)

-- Temporarily
import Network.Google.FusionTables 


{- Generate Overhead plots! -} 


cid  = "925399326325-6dir7re3ik7686p6v3kkfkf1kj0ec7ck.apps.googleusercontent.com" 
csec = "MQ72ZWDde_1e1ihI5YE9YlEi"  

table_name = "Dynaprof_Benchmarks" 


-- Maximum number of datapoints to use 
-- num_samples = 30 

-- What benchmark 
benchmark = "h264ref-9.3" 

-- what programs
programs = ["firstTimeInitOverheadZcaToggle",
            "firstTimeInitOverheadDynaProf"]

-- what computer
hostname = "delta" -- comes from different (but identical computers) 

variant = "none"


git_depth = "431" -- pull down only a small amount of the total data. 


main :: IO ()
main = do

  --tab <- pullEntireTable cid csec table_name
  tab <- pullSelectively cid csec table_name "GIT_DEPTH" git_depth 
  let (ColData cols values) = tab 

  -- all_data [[a]]
  let all_data = map (\x -> slice "PROGNAME" x cols values) programs 

      args'      = map (extractColumn "ARGS" cols) all_data
      args       = map (map convertInt) args' 
      run_times' = map (extractColumn "MEDIANTIME" cols) all_data
      run_times  = map (map convert) run_times'

      arg_rt = map average_at $ map sortIt $ zipWith (zip) args run_times

      -- 
      [zcadata',dynadata'] = arg_rt
      zcadata = drop 4 $ map (\(a,t) -> (a,t / fromIntegral a)) zcadata'
      dynadata = drop 4 $ map (\(a,t) -> (a,t / fromIntegral a)) dynadata'
      
      
      
  putStrLn "Showing filtered data"
  putStrLn "------------------------------------------------------------"
  putStrLn $ show args
  putStrLn "------------------------------------------------------------"
  putStrLn $ show run_times
  putStrLn "------------------------------------------------------------"
  putStrLn $ show arg_rt
  putStrLn "------------------------------------------------------------"
  putStrLn $ show zcadata
  putStrLn "------------------------------------------------------------"
  putStrLn $ show dynadata
  putStrLn "------------------------------------------------------------"

-- START THE PLOTING 


  let zcaGraph = LineGraph "#F00"
                 "ZcaToggle Init Overhead"
                 Nothing
                 zcadata -- first data series

  let dynaGraph = LineGraph "#0FF"
                  "DynaProf Init Overhead"
                  Nothing
                  dynadata
  

  let plot = Plot {pLines = [zcaGraph,dynaGraph],
                   pPoints = [],
                   pBars = [],
                   pLegend = True,
                   pDimensions = (800,400),
                   pXLabel = "ARGUMENT",
                   pYLabel = "s",
                   pXAxisTicks = Nothing,
                   pYAxisTicks = Nothing,
                   pXAxisLog = False,
                   pYAxisLog = False} 
                            
                                
                           
  putStrLn $ html $ renderPlot mySupply plot     


--------------------------------------------------------------------------
-- extract data given a variant

extractVariantMedianTime cols dat variant =
  let rows = slice "VARIANT" variant cols dat
  in  extractColumn "MEDIANTIME" cols rows 


-- extract the estimated overhead.
extractVariantOverhead cols dat variant =
  let rows = slice "VARIANT" variant cols dat
  in  extractColumn "FINAL_OVERHEAD" cols rows 


---------------------------------------------------------------------------
-- Utilities

nullFT (StringValue []) = True
nullFT _ = False


average_at :: [(Int,Double)] -> [(Int,Double)]
average_at xs  = result 
  
  where groups = groupBy (\(g,_) (g',_) -> g == g') xs 
        nubbed = nub $ concat $ map (map fst) groups
        avgs   = map average (map (map snd) groups)
        result = zip nubbed avgs 


sortIt r = sortBy (\(g,_) (g',_) -> compare g g') r

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



---------------------------------------------------------------------------
-- out as CSV  (Move to a library) 
