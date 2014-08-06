
{-

  For the resampling_x_y benchmarks

  make a clustered bar graph that

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

-- What benchmark 
benchmark = "h264ref-9.3" 

-- what computer
machine = "delta" -- "tank"

variantPrefix  = "resampling"

git_depth = "452" 


main :: IO ()
main = do

  --tab <- pullEntireTable cid csec table_name
  tab <- pullSelectively cid csec table_name "GIT_DEPTH" git_depth 
  
  let (ColData cols values) = tab 
   
  -- Collect all data of a given benchmark on a given machine. 
  let machinevals = slice "HOSTNAME" machine cols values 
      benchdata = slice "PROGNAME" benchmark cols machinevals
      variantdata  = sliceWithPrefix "VARIANT" variantPrefix cols benchdata
      variants = extractColumn "VARIANT" cols variantdata
      varianttups = map variantToTuple variants 

      run_times = map convert $ extractColumn "MEDIANTIME" cols variantdata 

      vr = zip varianttups run_times
      
      -- Sort by first of tuple
      s1 = sortBy (\((x,_),_) ((y,_),_) -> compare x y) vr
                  
      e1 = groupBy (\((x,_),_) ((y,_),_) -> x == y) s1

      s2 = map (sortBy (\(x,_) (y,_) -> compare x y)) e1 

      avgs = map average_at s2

  --get the baseline value
  let base_line_rt = average
                     $ map convert
                     $ extractColumn "MEDIANTIME" cols
                     $ slice "VARIANT" "unprofiled" cols benchdata 
  
  putStrLn $ "VALUE OF base_line_rt: " ++ show base_line_rt
  
  -- Some restructuring of the data 
  let groups = map (fst . fst . head) avgs
      -- Show y to switch the plotting tool into "category mode" 
      gdata  = map (map (\((_,y),z) -> (show y,percentage z))) avgs

      percentage x = 100 * ((x - base_line_rt) / base_line_rt)

  putStrLn "Showing" 
  putStrLn "------------------------------------------------------------"
  putStrLn $ show base_line_rt 
  putStrLn "------------------------------------------------------------"
  putStrLn $ show groups 
  putStrLn "------------------------------------------------------------"
  putStrLn $ show gdata
  putStrLn "------------------------------------------------------------"

  let colors = ["#F00","#FF0", "#000", "#0FF", "#0F0", "#00F"]
      
      
  let graphs = map (\(col,group,gdata) ->
                     BarGraph col ("sample_" ++ show group) gdata) $ drop 2 $ zip3 colors groups gdata
 

  let plot = Plot {pLines = [],
                   pPoints = [],
                   pBars = graphs,
                   pLegend = True,
                   pDimensions = (800,400),
                   pXLabel = "Sample epoch",
                   pYLabel = "Overhead %",
                   pXAxisTicks = Nothing,
                   pYAxisTicks = Nothing,
                   pXAxisLog = False,
                   pYAxisLog = False} 
                            
                                
                           
  putStrLn $ html $ renderPlot mySupply plot     



{- 
  -- Calc overhead percentage. 
  let overhead (g,x) =(g, (x - base) / base) -- ((x - base) / x)

      real_overheads = map (map overhead) avg_combo

  
  
 
-}
---------------------------------------------------------------------------
-- Variant to tuple
variantToTuple :: FTValue -> (Int,Double)
variantToTuple (StringValue str) = (convertInt (StringValue samplesize)
                                   ,convert (StringValue epoch))  
                     
  where [name,samplesize,epoch] = words $ map (\x -> if x == '_' then ' ' else x) str 
      

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


average_at :: Eq a =>  [(a,Double)] -> [(a,Double)]
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

sliceWithPrefix col valuePrefix header values = 
  [x | x <- values
     , let (StringValue str) = (x !! ix)
     , valuePrefix `isPrefixOf` str   ] 
  where
    
    ix = fromJust $ elemIndex col header 
    


---------------------------------------------------------------------------
-- Compute average of a list of positive double
-- Negative result means there is an error (probably missing data in fusiontable) 
average :: [Double] -> Double
average [] = -1
average xs = sum xs / (fromIntegral (length xs))
