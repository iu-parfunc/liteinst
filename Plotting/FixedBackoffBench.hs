
-- | Create a line-chart with one line per benchmark plotting fixed_backoff ONLY 


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


cid :: String
cid  = "925399326325-6dir7re3ik7686p6v3kkfkf1kj0ec7ck.apps.googleusercontent.com" 

csec :: String
csec = "MQ72ZWDde_1e1ihI5YE9YlEi"  

table_name = "Dynaprof_Benchmarks2" 

base_variant = "unprofiled" 

variantPrefix = "fixed_backoff_"


---------------------------------------------------------------------------
-- Plan

-- extract [( prog , [("1",value),("10",value)])), (prog, [("1",value),("10",value)])] 
--
--  so get all data in [(program, backoff, value)] then organize as above. 
-- 

-- 
                


main :: IO ()
main = do

  args <- getArgs

  -- Git_depth, Machine, progname

  when (length args /= 2) $ error "Provide exactly 3 arguments, GIT_DEPTH MACHINE PROGNAME" 
  
  let [git_depth, machine] = args 

  perform git_depth machine 


perform :: String -> String  -> IO ()
perform git_depth machine  = do 
  
  -- tab <- pullEntireTable cid csec table_name
  tab <- pullSelectively cid csec table_name "HOSTNAME" machine -- git_depth 


  -- extract the useful parts        
  let (ColData cols values) = tab 

  let filtered_by_gitdepth = aboveDepth git_depth cols values 
      
  let machine_data = filtered_by_gitdepth --slice "HOSTNAME" machine cols values    

 
  let fixedBackoff_data =  fixedBackoffExtractor
                           $ medianPerVariant
                           $ byBench
                           $ extractPbV variantPrefix cols machine_data


  let baseline_data = medianPerVariant $ 
                      byBench $
                      extractPbV "unprofiled"  cols machine_data


  let fixedBackoff_oh = computeOverheads baseline_data fixedBackoff_data
      
      
  putStrLn "---------------------------------------------------------------------------" 
  putStrLn $ show baseline_data

  putStrLn "---------------------------------------------------------------------------" 
  putStrLn $ show fixedBackoff_oh
  
  
  --let prog_vals = slice "PROGNAME" progname cols machine_data
    
  --when (null prog_vals) $ 
  --  hPutStrLn stderr "WARNING: no data for this benchmark from this machine at this git_depth"

  let colors  = ["#FF0", "#0FF", "#0F0", "#00F",
                 "#700","#750", "#705", "#70F", "#7F0", "#70F",
                 "#5A0","#55A", "#5A5", "#5AF", "#5FA", "#5AF"]


   
                
  -- START THE PLOTING 
  ---------------------------------------------------------------------------
  -- ALL THE RESAMPLING LINES
  let lines =
        map (\(c,(bench,ldata)) -> LineGraph c
                                   bench
                                   (Just "") 
                                   ldata) $ zip colors fixedBackoff_oh
        
  
  let plot = Plot {pLines = lines,
                   pPoints = [],
                   pBars = [],
                   pLegend = True,
                   pDimensions = (800,600),
                   pXLabel = "Backoff threshold",
                   pYLabel = "Overhead %",
                   pXAxisTicks = Just [1,10,100,1000,10000,100000,1000000],
                   pYAxisTicks = Nothing,
                   pXAxisLog = True,
                   pYAxisLog = False} 
                            
  let outfile = "FixedBackoff_" ++ machine ++ "_" ++ git_depth ++ ".html" 
             
  putStrLn $ "Writing output to " ++ outfile
  writeFile outfile $ html $ renderPlot mySupply plot     
  


  
      

{- 
  ---------------------------------------------------------------------------
  -- Extract baseline and gprof 
  let base_prog = extractVariantMedianTime cols prog_vals base_variant 

      base_prog_rts = medianOrErr "NO Unprofiled data!" $ map convert base_prog
   
      gprof_prog = overhead base_prog_rts
                   $ medianOrErr "No GProf data!"
                   $ map convert
                   $ extractVariantMedianTime cols prog_vals "gprof"

      no_backoff_prog  = overhead base_prog_rts
                         $ medianOrErr "No no_backoff data!" 
                         $ map convert
                         $ extractVariantMedianTime cols prog_vals "no_backoff"

  -- If we reach this point then all the above data should be fine! 
  --when (any (\x -> x<0) [base_prog_rts, gprof_prog, no_backoff_prog]) $
  --  putStrLn $ "WARNING: Generating graph with !STRANGE! set of data " ++
  --              show base_prog_rts ++ " " ++ show gprof_prog ++ " " ++ show no_backoff_prog



  ---------------------------------------------------------------------------
  -- Extract the resampling numbers 
  let resampling_data = extract2 cols prog_vals
      resampling_organized = organize resampling_data
      resampling_tmp = catMaybes $ map doneForPlot resampling_organized
      --resampling_avg = map (\(x,d) -> (x, average_at $ sortIt d)) resampling_tmp
      resampling_avg = map (\(x,d) -> (x, median_at d)) resampling_tmp
      resampling_done = map (\(x,d) -> (x,map (\(x,y) -> (x,overhead base_prog_rts y)) d)) resampling_avg

  when (null resampling_data) $
    putStrLn "WARNING: no resampling data for this benchmark" 

  putStrLn $ show resampling_done
  





  ---------------------------------------------------------------------------
  -- This is a mess! 
  let magic o xs =
        let s1 = sortBy (\(x,_) (y,_) -> compare x y) xs
                 -- e1 = groupBy (\(x,_) (y,_) -> x == y) s1
        in  map (\(a,b) -> (show a, overhead o b)) s1
  
  let prog_plotdata = magic base_prog_rts $ 
                      map (\v -> (variantToVal v,average $
                                                 map convert $
                                                 extractColumn "MEDIANTIME" cols (slice "VARIANT" v cols prog_vals))) variants
  
  putStrLn $ show prog_plotdata 
      
  
-} 
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

convertStr :: FTValue -> String
convertStr (DoubleValue v) = show v
convertStr (StringValue s) = s 

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
      times = extractColumn "MEDIANTIME" cols onlyResample
  in zip (map (\(StringValue n) -> n) names) (map convert times)


organize :: [(String, Double)] -> [[(String,Int,Double)]]
organize indata = map rearrangeGroup grouped                                    

  where

    expanded = map (\(x,y) -> (nameToTripple x,y)) indata
    sorted   = sortBy (\((_,_,x),_) ((_,_,y),_) -> x `compare` y) expanded 
    grouped  = groupBy (\((_,_,x),_) ((_,_,y),_) -> x == y) sorted 
    
    rearrangeGroup = map (\((_,backoff,s),v) -> (s, backoff, v)) 
    
    nameToTripple str = 
      let [name,samplesize,epoch] = words $ map (\x -> if x == '_' then ' ' else x) str 
      in (name,read samplesize :: Int ,epoch) 

doneForPlot :: [(String,Int,Double)] -> Maybe (String,[(Int,Double)])
doneForPlot [] = Nothing
doneForPlot xxs@((x,_,_):xs) = Just (x, map (\(a,b,c) -> (b,c)) xxs) 


---------------------------------------------------------------------------
-- extract, [(PROGNAME, backoff, Value)]

extractPbV :: String -> [String] -> [[FTValue]] -> [(String, String, Double)]
extractPbV variant  cols table =
  let onlyVariant = sliceWithPrefix "VARIANT" variant cols table
      variants  = extractColumn "VARIANT"    cols onlyVariant
      prgnames = extractColumn "PROGNAME"   cols onlyVariant
      values   = extractColumn "MEDIANTIME" cols onlyVariant
  in zip3 (map convertStr prgnames)
          (map convertStr variants)
          (map convert values) 
      

byBench :: [(String, String, Double)] -> [(String, [(String, Double)])]
byBench raw =
  let sorted = sortBy (\(x,_,_) (y,_,_) -> x `compare` y) raw
      grouped = groupBy (\(x,_,_) (y,_,_) -> x == y) sorted

  in catMaybes $ map groupRepr grouped     
  where 
    groupRepr [] = Nothing 
    groupRepr xs@((x,_,_):_) = Just (x, map (\(_,y,z) -> (y,z)) xs)



medianPerVariant :: [(String, [(String, Double)])] -> [(String , [(String, Double)])] 
medianPerVariant = map (\(x,d) -> (x, fixBenchData d))
  where 
    fixBenchData :: [(String,Double)] -> [(String,Double)]
    fixBenchData raw =
      let sorted = sortBy (\(x,_) (y,_) -> x `compare` y) raw
          medians = median_at sorted
      in medians 


fixedBackoffExtractor :: [(String, [(String, Double)])] -> [(String, [(Int, Double)])]
fixedBackoffExtractor = map (\(x,d) -> (x, sortBy (\(x,_) (y,_) -> x `compare` y) (doFixed d)))
  where
    doFixed :: [(String, Double)] -> [(Int, Double)]
    doFixed = map doTweak

    doTweak (str,d) =
      let [_,_,boff] = words $ map (\x -> if x == '_' then ' ' else x) str
      in  (read boff, d) 
     



---------------------------------------------------------------------------
--    

computeOverheads baseline_data [] = [] 
computeOverheads baseline_data ((bench,dat):rest) =
  case (lookup bench baseline_data) of
    Nothing -> computeOverheads baseline_data rest -- error $ "Baseline value is missing for Bench: " ++ show bench
    Just [] -> computeOverheads baseline_data rest --  error $ "Baseline value is missing for Bench: " ++ show bench
    Just [(_,base)] -> (bench, map applyOH dat):  computeOverheads baseline_data rest
      where
        applyOH (boff,rt) = (boff, overhead base rt) 
