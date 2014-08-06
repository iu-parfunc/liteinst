
-- | Create a per-benchmark line-chart plotting 

import System.Environment (getArgs)

import HSBencher.Analytics

import Control.Monad 

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

base_variant = "unprofiled" 

variants = ["fixed_backoff_" ++ x| x <- backoff ] 
 
backoff = [ "1", "10", "100", "1000", "10000", "100000", "1000000"]



main :: IO ()
main = do

  args <- getArgs

  -- Git_depth, Machine, progname

  when (length args /= 3) $ error "Provide exactly 3 arguments, GIT_DEPTH MACHINE PROGNAME" 
  
  let [git_depth, machine, progname] = args 

  perform git_depth machine progname



  
perform :: String -> String -> String -> IO ()
perform git_depth machine  progname = do 
  
  --tab <- pullEntireTable cid csec table_name
  tab <- pullSelectively cid csec table_name "GIT_DEPTH" git_depth 

  -- extract the useful parts        
  let (ColData cols values) = tab 

  let machine_data = slice "HOSTNAME" machine cols values    

  let prog_vals = slice "PROGNAME" progname cols machine_data
    
  when (null prog_vals) $ 
    putStrLn "WARNING: no data for this benchmark from this machine at this git_depth"

  let base_prog = extractVariantMedianTime cols prog_vals base_variant 

      base_prog_rts = average $ map convert base_prog
 
      gprof_prog = overhead base_prog_rts $ average $ map convert $ extractVariantMedianTime cols prog_vals "gprof"

      no_backoff_prog  = overhead base_prog_rts $ average $ map convert $ extractVariantMedianTime cols prog_vals "no_backoff"
  
  when (any (\x -> x<0) [base_prog_rts, gprof_prog, no_backoff_prog]) $
    putStrLn $ "WARNING: Generating graph with !STRANGE! set of data " ++
                show base_prog_rts ++ " " ++ show gprof_prog ++ " " ++ show no_backoff_prog
  
  let magic o xs = let s1 = sortBy (\(x,_) (y,_) -> compare x y) xs
                     -- e1 = groupBy (\(x,_) (y,_) -> x == y) s1
                 in  map (\(a,b) -> (show a, overhead o b)) s1
  
  let prog_plotdata = magic base_prog_rts $ map (\v -> (variantToVal v,average $ map convert $ extractColumn "MEDIANTIME" cols (slice "VARIANT" v cols prog_vals))) variants
  
  putStrLn $ show prog_plotdata 
      
  let colors  = ["#000","#F00", "#FF0", "#0FF", "#0F0", "#00F"]
      colors2 = ["#700","#750", "#705", "#70F", "#7F0", "#70F"]
      colors3 = ["#5A0","#55A", "#5A5", "#5AF", "#5FA", "#5AF"]
  -- START THE PLOTING 

  let dyna_lines = [(progname,prog_plotdata)] 

  let lines =
        map (\(c,(n,x)) -> LineGraph c
                          ("dynaprof_" ++ n)
                          Nothing
                          x ) $ zip colors dyna_lines 

  let gp_lines = [(progname,  zip backoff (repeat gprof_prog))]

  
  let lines2 =
        map (\(c,(n,x)) -> LineGraph c
                          ("gprof_" ++ n)
                          Nothing
                          x ) $ zip colors2 gp_lines 

        
  let nb_lines = [(progname, zip backoff (repeat no_backoff_prog))] 
      
  let lines3 =
        map (\(c,(n,x)) -> LineGraph c
                           ("no_backoff_" ++ n)
                           Nothing
                           x ) $ zip colors3 nb_lines

        
  
  let plot = Plot {pLines = lines ++ lines2 ++ lines3,
                   pPoints = [],
                   pBars = [],
                   pLegend = True,
                   pDimensions = (800,600),
                   pXLabel = "backoff",
                   pYLabel = "Overhead %"} 
                            
  let outfile = "BenchOH2_" ++ progname ++"_" ++ machine ++ "_" ++ git_depth ++ ".html" 
             
  putStrLn $ "Writing output to " ++ outfile
  writeFile outfile $ html $ renderPlot mySupply plot     
 

overhead base x = if x < 0 then -1 else 100 * ((x - base) / base)


---------------------------------------------------------------------------
-- Variant to tuple
variantToTuple :: String -> (Int,Double)
variantToTuple str = (convertInt (StringValue samplesize)
                     ,convert (StringValue epoch))  
                     
  where [name,samplesize,epoch] = words $ map (\x -> if x == '_' then ' ' else x) str 


variantToVal :: String -> Int
variantToVal str = convertInt (StringValue backoff)
  where [name1,name2,backoff] = words $ map (\x -> if x == '_' then ' ' else x) str 


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


