

import System.Environment (getArgs)

import HSBencher.Analytics


import Data.List hiding (lines)
import Data.Maybe
import Prelude hiding (log, lines)

-- Temporarily
import Network.Google.FusionTables 

cid  = "925399326325-6dir7re3ik7686p6v3kkfkf1kj0ec7ck.apps.googleusercontent.com" 
csec = "MQ72ZWDde_1e1ihI5YE9YlEi"  

table_name = "Dynaprof_Benchmarks" 



main :: IO ()
main = do

  --tab <- pullEntireTable cid csec table_name
  --tab <- pullSelectively cid csec table_name "GIT_DEPTH" "266"
  tab <- pullSelectively cid csec table_name "RUNID" "hive_1405445312"
 
  putStrLn "Showing all recieved data"
  putStrLn "------------------------------------------------------------"
  putStrLn $ show tab
  putStrLn "------------------------------------------------------------"

  let (ColData cols values) = tab 

  let gzip_data = slice "PROGNAME" "gzip16" cols values
      
      unprofiled_rows = slice "VARIANT" "unprofiled" cols gzip_data
      unprofiled_values = extractColumn "MEDIANTIME" cols unprofiled_rows

      gprof_rows = slice "VARIANT" "gprof" cols gzip_data
      gprof_values = extractColumn "MEDIANTIME" cols gprof_rows

      dyna05_rows = slice "VARIANT" "dynaprof_direct_05" cols gzip_data
      dyna05_values = extractColumn "MEDIANTIME" cols dyna05_rows

      dyna50_rows = slice "VARIANT" "dynaprof_direct_50" cols gzip_data
      dyna50_values = extractColumn "MEDIANTIME" cols dyna50_rows

      
  
  
      
  
  putStrLn "Showing values"
  putStrLn "------------------------------------------------------------"
  putStrLn $ show unprofiled_values
  putStrLn $ show gprof_values
  putStrLn $ show dyna05_values
  putStrLn $ show dyna50_values     
  putStrLn "------------------------------------------------------------"

  let dat = unprofiled_values ++ gprof_values ++ dyna05_values ++ dyna50_values
      tag = ["unprofiled", "gprof", "dyna05", "dyna50"]

  let the_graph = BarGraph "#F00"
                  "gzip16"
                  $zip tag (map convert dat) 

  -- let graph1 =
  --       BarGraph "#F00"
  --                "unprofiled"
  --                $zip (repeat "unprofiled") (map convert unprofiled_values)

  -- let graph2 =
  --       BarGraph "#FF0"
  --                "gprof"
  --                $zip (repeat "gprof") (map convert gprof_values)


  -- let graph3 =
  --       BarGraph "#0F0"
  --                "dyna05"
  --                $zip (repeat "dyna05") (map convert dyna05_values)

  -- let graph4 =
  --       BarGraph "#00f"
  --                "dyna50"
  --                $zip (repeat "dyna50") (map convert dyna50_values)


  let plot = Plot {pLines = [],
                   pPoints = [],
                   pBars = [the_graph],
                   pLegend = True,
                   pDimensions = (800,400),
                   pXLabel = "Tool",
                   pYLabel = "s"} 
                            
                                
                           
  putStrLn $ html $ renderPlot mySupply plot     

  

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
