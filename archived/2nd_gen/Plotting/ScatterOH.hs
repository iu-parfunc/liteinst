
-- | Create a scatter plot of NUM_SAMPLES vs Overhead(diff_time)

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

table_name = "Dynaprof_Benchmarks2" 

base_variant = "unprofiled" 

variantPrefixes = ["fixed_backoff", "resampling", "no_backoff"]

prognames = ["perl-5.8.7", "raxml" , "gzip-1.6", "h264ref-9.3", "bzip-1.0.3", "grep-2.18" ] -- benchmarks of interest.



---------------------------------------------------------------------------
-- Plan
--
-- pull out (PROGNAME, NUM_SAMPLES, MEDIANTIME) tripples for each variant
-- pull out baseline value for each PROGMAME
-- Plot NUM_SAMPLES vs (diff MEDIANTIME baseline) 

main :: IO ()
main = do

  args <- getArgs

  -- Git_depth, Machine, progname

  when (length args /= 2) $ error "Provide exactly 2 arguments, GIT_DEPTH MACHINE" 
  
  let [git_depth, machine] = args 

  perform git_depth machine



  
perform :: String -> String -> IO ()
perform git_depth machine = do 
  
  --tab <- pullSelectively cid csec table_name "GIT_DEPTH" git_depth
  tab <- pullSelectively cid csec table_name "HOSTNAME" machine 

  -- extract the useful parts        
  let (ColData cols values) = tab
      
  let filtered_by_gitdepth = aboveDepth git_depth cols values

  let machine_data = filtered_by_gitdepth -- slice "HOSTNAME" machine cols values    

  when (null machine_data) $ error "No data from this machine at this GIT_DEPTH!" 
  
  ---------------------------------------------------------------------------
  -- Ok we have data at a specific depth and machine.
  
  let prog_vals = sliceL "PROGNAME" prognames cols machine_data
    
  when (null prog_vals) $ 
    putStrLn "WARNING: no data for any benchmark from this machine at this git_depth"


  let bench_tripples = extractTripples prognames cols prog_vals

  putStrLn $ show bench_tripples

  let bench_base = map (\(x,y) -> (x, average $ map convert y)) 
                   $ map (\p -> (p, extractVariantMedianTime cols (slice "PROGNAME" p cols prog_vals) "unprofiled")) prognames

  putStrLn "---------------------------------------------------------------------------"
  putStrLn "Showing the base_line" 
  putStrLn $ "\n\n" ++ show bench_base

  ---------------------------------------------------------------------------
  -- Clean out NaNs

  
  let cleaner (n,xys) = (n, filter (\(x,y) -> x /= (StringValue "NaN")) xys)


      tripples_cleaned = map cleaner bench_tripples 

  putStrLn "---------------------------------------------------------------------------"
  putStrLn "Showing cleaned tripples" 
  putStrLn $ show tripples_cleaned


  ---------------------------------------------------------------------------
  -- prepare for plotting

  let tripples_num = map (\(n,xys) -> (n,map (\(x,y) -> (convertInt x, convert y)) xys)) tripples_cleaned

      compute_diff [] = []
      compute_diff ((n,xys):rest) =
        case lookup n bench_base of
          Nothing -> compute_diff rest
          Just br -> (n, map (\(s,t) -> (s,t - br)) xys): compute_diff rest 
      
      tripples_diff = compute_diff tripples_num 

  putStrLn "---------------------------------------------------------------------------"
  putStrLn "Showing diffed tripples" 
  putStrLn $ show tripples_diff

  let points = concatMap snd tripples_diff
  
{-       
  let colors  = ["#000","#F00", "#FF0", "#0FF", "#0F0", "#00F"]
      colors2 = ["#700","#750", "#705", "#70F", "#7F0", "#70F"]
      colors3 = ["#5A0","#55A", "#5A5", "#5AF", "#5FA", "#5AF"]
  -- START THE PLOTING 

  let plot_data = PointGraph "#F00"
                             []
                             []
                             points 
      
  let plot = Plot {pLines = [],
                   pPoints = [plot_data],
                   pBars = [],
                   pLegend = True,
                   pDimensions = (800,600),
                   pXLabel = "NUM_SAMPLES",
                   pYLabel = "Overhead s",
                   pXAxisTicks = Just [1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000],
                   pYAxisTicks = Just [1,10,100,1000],
                   pXAxisLog = True,
                   pYAxisLog = True} 
                            
  let outfile = "ScatterOH.html" 
             
  putStrLn $ "Writing output to " ++ outfile
  writeFile outfile $ html $ renderPlot mySupply plot     
 -} 

  let schema = "samples overhead\n" 
      string = concatMap (\(x,y) -> show x ++ " " ++ show y ++ "\n" ) points 


  writeFile "scatterdat.txt" $ schema ++ string

  
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
-- extract data given a variant (copy paste programming)

extractVariantMedianTime :: [String] -> [[FTValue]] -> String -> [FTValue]
extractVariantMedianTime cols dat variant =
  let rows = slice "VARIANT" variant cols dat
  in  extractColumn "MEDIANTIME" cols rows 

extractProgramMedianTimeP :: [String] -> [[FTValue]] -> String -> [FTValue]
extractProgramMedianTimeP cols dat variant =
  let rows = sliceWithPrefix "PROGNAME" variant cols dat
  in  extractColumn "MEDIANTIME" cols rows 


-- extract the estimated overhead.
extractVariantOverhead :: [String] -> [[FTValue]] -> String -> [FTValue]
extractVariantOverhead cols dat variant =
  let rows = slice "VARIANT" variant cols dat
  in  extractColumn "FINAL_OVERHEAD" cols rows 

extractVariantNumSamples :: [String] -> [[FTValue]] -> String -> [FTValue]
extractVariantNumSamples cols dat variant =
  let rows = slice "VARIANT" variant cols dat
  in  extractColumn "NUM_SAMPLES" cols rows 

extractProgramNumSamplesP :: [String] -> [[FTValue]] -> String -> [FTValue]
extractProgramNumSamplesP cols dat variant =
  let rows = sliceWithPrefix "PROGNAME" variant cols dat
  in  extractColumn "NUM_SAMPLES" cols rows 


--
extractTripples :: [String] -> [String] -> [[FTValue]] -> [(String, [(FTValue,FTValue)])]
extractTripples [] cols table = []
extractTripples (prg:prgs) cols table =
  let mt = extractProgramMedianTimeP cols table prg
      sc = extractProgramNumSamplesP cols table prg
  in (prg, zip sc mt) : extractTripples prgs cols table     
  
  

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
   


sliceL col sliceout header values =
  [x | x <- values , or [v `isPrefixOf` str | let (StringValue str) = x !! ix, v <- sliceout] ] 
  where
    ix = fromJust $ elemIndex col header 
  

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
    

aboveDepth :: String -> [String] -> [[FTValue]] -> [[FTValue]] 
aboveDepth str header table =
  [ x | x <- table
      , let (StringValue depth) = (x !! ix)
      , (read depth :: Int) >= (read str :: Int)] -- gah! 
  where
    ix = fromJust $ elemIndex "GIT_DEPTH" header

---------------------------------------------------------------------------
-- Compute average of a list of positive double
-- Negative result means there is an error (probably missing data in fusiontable) 
average :: [Double] -> Double
average [] = -1
average xs = sum xs / (fromIntegral (length xs))

