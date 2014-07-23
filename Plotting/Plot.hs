
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

machine = "hive" -- "tank"
git = "306"

benchmark = "h264ref-9.3" -- "gzip" 

variants = [ "no_backoff"
           , "fixed_backoff_1000000"
           , "fixed_backoff_10000"
           , "bop_simple_05_10000"
           , "bop_simple_50_10000"
           , "gprof"
           , "unprofiled"
           ]  

main :: IO ()
main = do

  --tab <- pullEntireTable cid csec table_name
  --tab <- pullSelectively cid csec table_name "GIT_DEPTH" "266"

  tab <- pullSelectively cid csec table_name "GIT_DEPTH" git
         

  
  putStrLn "Showing all recieved data"
  putStrLn "------------------------------------------------------------"
  putStrLn $ show tab
  putStrLn "------------------------------------------------------------"

  -- Get the column names into cols and the values of all those
  -- colums into values. (that is, values is the entire table)
  -- and cols is a list of strings naming each column. 
  let (ColData cols values) = tab 

  -- Collec all data of a given benchmark on a given machine. 
  let allData =
        slice "HOSTNAME" machine cols 
          (slice "PROGNAME" benchmark cols values) 
  
  -- If there are more than one value at this "git_depth", average them out.
  let medianTimes = map (extractVariantMedianTime cols allData) variants
      -- Convert to doubles and average if there are many values 
      averages = map average $ map (map convert) medianTimes  
      varTime = zip variants averages            
  
      
  putStrLn "Showing values"
  putStrLn "------------------------------------------------------------"
  putStrLn $ show varTime
  putStrLn "------------------------------------------------------------"


  -- The data series that generate one set of bars in the same color 
  let the_graph = BarGraph "#F00"
                  "gzip16"
                  varTime

  -- The Plot   
  let plot = Plot {pLines = [],
                   pPoints = [],
                   pBars = [the_graph],
                   pLegend = True,
                   pDimensions = (800,400),
                   pXLabel = "Tool",
                   pYLabel = "s"} 
                            
                                
  -- Generate the HTML and Javascript code                           
  putStrLn $ html $ renderPlot mySupply plot     


--------------------------------------------------------------------------
-- extract data given a variant

extractVariantMedianTime cols dat variant =
  let rows = slice "VARIANT" variant cols dat
  in  extractColumn "MEDIANTIME" cols rows 

  

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

