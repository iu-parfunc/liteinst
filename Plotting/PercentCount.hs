
{-
   Repurposing this old plot for
   the percentage / count plot

-} 

import System.Environment (getArgs)
import System.FilePath.Posix

import HSBencher.Analytics

import Control.Monad

import Data.List hiding (lines)
import Data.Maybe
import Prelude hiding (log)

import Data.List.Split (wordsBy) 

extract str = (abs (read s1 :: Int), read s2 :: Int) 
  where
    [s1,s2] = wordsBy (==',') str 


-- For other setups HACK THIS PART!! 
benches = ["h264", "grep", "gzip", "bzip2"] 

compensated_benches = map (++"_compensated.txt") benches
non_compensated_benches = map (++"_non_compensated.txt") benches 



main :: IO ()
main = do

  --args <- getArgs

  --when (length args == 0) $ error "Needs a "  
  
  --let fn = args  !! 0 
  
  --let f i = (30 :: Int)
  -- Percent/count mapping 
  --let csv = unlines $ [ show x ++ ", " ++ show (f x)  | x <- [1..100]] 
  
  
  -- csv <- readFile "leaf_Aug4.csv"
  csv_compensated <- mapM readFile compensated_benches 
  csv_non_compensated <- mapM readFile non_compensated_benches 
                     
  let the_compensated_lines =  map process csv_compensated
      the_non_compensated_lines = map process csv_non_compensated 

  let colors = ["#000","#F00", "#FF0", "#07F", "#F0F", "#00F"]
  
  let the_compensated_graph
        = map (\(c,bench,ls) ->  
                LineGraph c
                bench
                Nothing
                ls)  $ zip3 colors benches the_compensated_lines 

  
  -- The Plots
  let plot_compensated =
        Plot {pLines = the_compensated_graph,
              pPoints = [],
              pBars = [],
              pLegend = True,
              pDimensions = (800,400), -- unused! 
              pXLabel = "Elapsed time difference (%)",
              pYLabel = "Cumulative % functions",
              pXAxisTicks = Nothing,
              pYAxisTicks = Nothing,
              pXAxisLog = False,
              pYAxisLog = False}

  let the_non_compensated_graph
        = map (\(c,bench,ls) ->  
                LineGraph c
                bench
                Nothing
                ls)  $ zip3 colors benches the_non_compensated_lines 

  let plot_non_compensated =
        Plot {pLines = the_non_compensated_graph,
              pPoints = [],
              pBars = [],
              pLegend = True,
              pDimensions = (800,400), -- unused! 
              pXLabel = "Elapsed time difference (%)",
              pYLabel = "Cumulative % functions",
              pXAxisTicks = Nothing,
              pYAxisTicks = Nothing,
              pXAxisLog = False,
              pYAxisLog = False}
  
                            
                                
  -- Generate the HTML and Javascript code                           
  -- putStrLn $ html $ renderPlot mySupply plot

  writeFile "compensated.html" $ html $ renderPlot mySupply plot_compensated
  writeFile "non_compensated.html" $ html $ renderPlot mySupply plot_non_compensated
  
    
  --let (_,nom) = splitFileName fn
  --    ofile = "output_" ++ (dropExtension nom) ++ ".html" 
  --putStrLn $ "Writing output to: " ++ ofile
  --writeFile ofile $ html $ renderPlot mySupply plot
  


  
process csv = 
  -- percent/count mapping
  let the_data = sortBy (\(x,_) (y,_) -> x `compare` y) [ extract l | l <- lines csv]

      ps  = map fst the_data -- percentages
      cs  = map snd the_data -- counts
      
      totc = sum cs -- total op f all "counts"
      
      pcs = map (\x -> 100 * ((fromIntegral x) / (fromIntegral totc))) cs 
      accCount = scanl1 (+) pcs 
  in  zip ps accCount :: [(Int,Double)]
        

 --  putStrLn $ show the_data 
   
{-   
  -- The data series that generate one set of bars in the same color 
 
-}   
  
--------------------------------------------------------------------------
-- extract data given a variant

-- extractVariantMedianTime cols dat variant =
--   let rows = slice "VARIANT" variant cols dat
--   in  extractColumn "MEDIANTIME" cols rows 

  

--------------------------------------------------------------------------
-- Converting and Slicing.
-- This should be improved and placed in the library. 

{- 
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

-}
