
-- | Create a per-benchmark line-chart plottin resampling overheads

import System.Environment (getArgs)
import System.IO 

import HSBencher.Analytics

import Control.Monad 

import Data.List hiding (lines)
import Data.Maybe
import Prelude hiding (log, lines)

-- Temporarily
import Network.Google.FusionTables 
-- import Network.Google (FTString)

{- Generate Overhead plots! -} 


-- HACKY SWITCH
-- _USE_TIME = "MEDIANTIME" -- "MINTIME" -- "MEDIANTIME"  
 
cid :: String
cid  = "925399326325-6dir7re3ik7686p6v3kkfkf1kj0ec7ck.apps.googleusercontent.com" 

csec :: String
csec = "MQ72ZWDde_1e1ihI5YE9YlEi"  

-- Switching to the new table. 
table_name = "Dynaprof_Benchmarks2" 

base_variant = "unprofiled" 

-- variants = ["fixed_backoff_" ++ x| x <- backoff ] 

            -- skip last backoff variant (since the resampling one do not go that high 
-- backoff = [ "1", "10", "100", "1000", "10000", "100000"] -- , "1000000"]

-- variants2_prefix = ["rasampling_" ++ x | x <- backoff] 


-- extract2 hardcodes extraction of resampling data... so should be fine. 
-- Get all resampling data! 
-- variants_prefix = "resampling"



---------------------------------------------------------------------------
-- Plan

-- extract [( epoch , [("1",value),("10",value)])), (epoch, [("1",value),("10",value)])] 
--
--  so get all data in [(backoff, epoch, value)] then organize as above. 
-- 


main :: IO ()
main = do

  args <- getArgs

  -- Git_depth, Machine, progname

  when (length args /= 3) $  error "Provide exactly 3 arguments, GIT_DEPTH MACHINE PROGNAME"  

  
  let [git_depth, machine, progname]  = args 

  -- putStrLn $ "Settings \n" ++ 
  --            "Git_depth: " ++ git_depth ++ "\n"
  --            "Machine:   " ++ machine   ++ "\n"
  --            "Bench:     " ++ progname  
             
  putStrLn $ "Pulling all relevant data at gitdepth " ++ git_depth ++ " and above" 
      
  perform git_depth machine progname 



perform :: String -> String -> String -> IO ()
perform git_depth machine  progname = do 
  
  -- tab <- pullEntireTable cid csec table_name
  tab <- pullSelectively cid csec table_name "HOSTNAME" machine -- git_depth 


  -- extract the useful parts        
  let (ColData cols values) = tab 

  let filtered_by_gitdepth = aboveDepth git_depth cols values 
      
  let machine_data = filtered_by_gitdepth -- slice "HOSTNAME" machine cols values    

  let prog_vals = slice "PROGNAME" progname cols machine_data
    
  when (null prog_vals) $ 
    putStrLn "WARNING: no data for this benchmark from this machine at this git_depth"

  ---------------------------------------------------------------------------
  -- Extract baseline and gprof 
  let base_prog = extractVariantMedianTime cols prog_vals base_variant 

      base_prog_rts = medianOrErr "NO Unprofiled data!" $ map convert base_prog

  -- DO NOT EXTRACT gprod AND no_backoff data
      
      -- gprof_prog = overhead base_prog_rts
      --              $ medianOrErr "No GProf data!"
      --              $ map convert
      --              $ extractVariantMedianTime cols prog_vals "gprof"

      -- no_backoff_prog  = overhead base_prog_rts
      --                    $ medianOrErr "No no_backoff data!" 
      --                    $ map convert
      --                    $ extractVariantMedianTime cols prog_vals "no_backoff"

  -- If we reach this point then all the above data should be fine! 
  --when (any (\x -> x<0) [base_prog_rts, gprof_prog, no_backoff_prog]) $
  --  putStrLn $ "WARNING: Generating graph with !STRANGE! set of data " ++
  --              show base_prog_rts ++ " " ++ show gprof_prog ++ " " ++ show no_backoff_prog



  ---------------------------------------------------------------------------
  -- Extract the resampling numbers
  -- TODO: update this 
  let resampling_data = extract2 cols prog_vals
      resampling_organized = organize resampling_data
      resampling_tmp = catMaybes $ map doneForPlot resampling_organized
      --resampling_avg = map (\(x,d) -> (x, average_at $ sortIt d)) resampling_tmp
      resampling_avg = map (\(x,d) -> (x, median_at d)) resampling_tmp
      resampling_done' = map (\(x,d) -> (x,map (\(x,y) -> (x,overhead base_prog_rts y)) d)) resampling_avg

      resampling_done = take (length resampling_done' - 2) resampling_done'
  
  when (null resampling_data) $
    putStrLn "WARNING: no resampling data for this benchmark" 

  hPutStrLn stderr "---------------------------------------------------------------------------"
  hPutStrLn stderr "-- RESAMPLING VALUES "
  hPutStrLn stderr $ show resampling_done
  





  ---------------------------------------------------------------------------
  -- This is a mess! 
  let magic o xs =
        let s1 = sortBy (\(x,_) (y,_) -> compare x y) xs
                 -- e1 = groupBy (\(x,_) (y,_) -> x == y) s1
        in  map (\(a,b) -> (show a, overhead o b)) s1

  -- WE ARE CURRENTLY NOT INTERESTED IN THIS DATA 
  -- This is fixed_backoff data (Variants = ["fixed_backoff_x",....]
  --let prog_plotdata = magic base_prog_rts $ 
  --                    map (\v -> (variantToVal v,average $
  --                                               map convert $
  --                                               extractColumn "MINTIME" cols (slice "VARIANT" v cols prog_vals))) variants

  -- hPutStrLn stderr "---------------------------------------------------------------------------"
  -- hPutStrLn stderr "-- Prog Plot Data "  
  -- putStrLn $ show prog_plotdata 
      
  let colors  = ["#FF0", "#0FF", "#0F0", "#00F",
                 "#700","#750", "#705", "#70F", "#7F0", "#70F",
                 "#5A0","#55A", "#5A5", "#5AF", "#5FA", "#5AF"]
  -- START THE PLOTING 

  ---------------------------------------------------------------------------
  -- FIXED_BACKOFF LINE 
  --let dyna_lines = [(progname,prog_plotdata)] 

  -- let lines = LineGraph "#000" 
  --                       "dynaprof_fixed_backoff"
  --                       (Just "") 
  --                       prog_plotdata -- ) $ zip colors dyna_lines 


  ---------------------------------------------------------------------------
  -- GPROF LINE
  --let gp_lines = [(progname,  zip backoff (repeat gprof_prog))]
  
  -- let lines2 = LineGraph "#AAA" 
  --                        "gprof"
  --                        (Just "") 
  --                        (zip backoff (repeat gprof_prog)) --  x ) $ zip colors2 gp_lines 




  ---------------------------------------------------------------------------
  -- NO BACKOFF LINE
  --let nb_lines = [(progname, zip backoff (repeat no_backoff_prog))] 
      
  -- let lines3 = LineGraph "#F00"
  --                        "dynaprof_no_backoff"
  --                        (Just "") 
  --                        (zip backoff (repeat no_backoff_prog)) -- x ) $ zip colors3 nb_lines

  ---------------------------------------------------------------------------
  -- ALL THE RESAMPLING LINES
  let lines4 =
        map (\(c,(n,ldata)) -> LineGraph c
                                ("resampling_" ++ n)
                                (Just "") 
                                (map (\(x,y) -> (show x,y)) ldata)) $ zip colors resampling_done 
        
  
  let plot = Plot {pLines = lines4,   -- ++  [lines,lines2,lines3]
                   pPoints = [],
                   pBars = [],
                   pLegend = True,
                   pDimensions = (800,600),
                   pXLabel = "Backoff threshold",
                   pYLabel = "Overhead %",
                   pXAxisTicks = Nothing,
                   pYAxisTicks = Nothing,
                   pXAxisLog = False,  -- DONT USE THE LOG PLOTS 
                   pYAxisLog = False}  -- DONT USE THE LOG PLOTS  
                            
  let outfile = "ResampleOverheadPlotNew_" ++ progname ++"_" ++ machine ++ "_" ++ git_depth ++ ".html" 
             
  putStrLn $ "Writing output to " ++ outfile
  writeFile outfile $ html $ renderPlot mySupply plot     

  ---------------------------------------------------------------------------
  -- Output txt file with:
  -- # benchName min max  gprof_min gprof_max fixed_min fixed_max  

  -- data to use resampling done, no_backoff_prog, gprof_prog
  -- let cut_down = take 3 $ drop 4 $ map snd resampling_done
  --     samplingValues = if (length cut_down < 3)
  --                      then error "Missing data!"
  --                      else map snd $ concat $ cut_down
  --     min_val = minimum samplingValues
  --     max_val = maximum samplingValues

  --     fixed_backoff_values =  map snd prog_plotdata
  --     fixed_backoff_min_val = minimum fixed_backoff_values
  --     fixed_backoff_max_val = maximum fixed_backoff_values

  --     no_backoff_min_val = no_backoff_prog
  --     no_backoff_max_val = no_backoff_prog
  
  --     gprof_min_val = gprof_prog
  --     gprof_max_val = gprof_prog
      
  --     string = progname ++ " " ++ (concat $ intersperse " " $ map show [min_val, max_val
  --                                                                      , no_backoff_min_val
  --                                                                      , no_backoff_max_val
  --                                                                      , fixed_backoff_min_val
  --                                                                      , fixed_backoff_max_val
  --                                                                      , gprof_min_val
  --                                                                      , gprof_max_val]) ++ "\n" 
  --     schema = "# PROGRAM resampling_min resampling_max no_backoff_min no_backoff_max fixed_backoff_min fixed_backoff_max gprof_min gprof_max \n" 
  -- -- error $ show string  
               
  -- let txtout = "BenchOH3_" ++ progname ++ "_" ++ machine ++ "_" ++ git_depth ++ ".txt"
  -- writeFile txtout string
  -- writeFile "schema.txt" schema
  
  

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
  in  extractColumn "MINTIME" cols rows 


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

median :: [Double] -> Maybe Double
median [] = Nothing
median [x] = Just x
median xs | odd n = Just $ sorted !! mid
          | otherwise = Just $ (sorted !! mid + sorted !! (mid - 1)) / 2 
  where
    sorted = sort xs
    n   = length xs 
    mid = n `div` 2

median_at :: (Ord a, Eq a) =>  [(a,Double)] -> [(a,Double)]
median_at xs  = result 
  
  where sorted = sortBy  (\(g,_) (g',_) -> g `compare` g') xs  
        groups = groupBy (\(g,_) (g',_) -> g == g') sorted
        nubbed = nub $ concat $ map (map fst) groups -- labels in right order!  
        avgs   = map median (map (map snd) groups)
        avgs_filtered = map fromJust
                      $ filter (not . isNothing) avgs 
        result = zip nubbed avgs_filtered 


medianOrErr :: String -> [Double] -> Double
medianOrErr str xs = maybe (error str) id $ median xs 



---------------------------------------------------------------------------
-- RESAMPLE variants extraction ... really messy stuff!

extract2 :: [String] -> [[FTValue]] -> [(String, Double)]
extract2 cols table =
  let onlyResample = sliceWithPrefix "VARIANT" "resampling" cols table
      names = extractColumn "VARIANT" cols onlyResample
      times = extractColumn "MINTIME" cols onlyResample
  in zip (map (\(StringValue n) -> n) names) (map convert times)


organize :: [(String, Double)] -> [[(String,Int,Double)]]
organize indata = map rearrangeGroup grouped                                    

  where

    expanded = map (\(x,y) -> (nameToTripple x,y)) indata
    sorted   = sortBy (\((_,_,x),_) ((_,_,y),_) -> (read x :: Double) `compare` (read y :: Double) ) expanded 
    grouped  = groupBy (\((_,_,x),_) ((_,_,y),_) -> x == y) sorted 
    
    rearrangeGroup = map (\((_,backoff,s),v) -> (s, backoff, v)) 
    
    nameToTripple str = 
      let [name,samplesize,epoch] = words $ map (\x -> if x == '_' then ' ' else x) str 
      in (name,read samplesize :: Int ,epoch) 

doneForPlot :: [(String,Int,Double)] -> Maybe (String,[(Int,Double)])
doneForPlot [] = Nothing
doneForPlot xxs@((x,_,_):xs) = Just (x, map (\(a,b,c) -> (b,c)) xxs) 