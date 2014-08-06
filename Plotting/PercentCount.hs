
{-
   Repurposing this old plot for
   the percentage / count plot

-} 

import System.Environment (getArgs)

import HSBencher.Analytics

import Control.Monad

import Data.List hiding (lines)
import Data.Maybe
import Prelude hiding (log)

import Data.List.Split (wordsBy) 

extract str = (read s1 :: Int, read s2 :: Int) 
  where
    [s1,s2] = wordsBy (==',') str 


main :: IO ()
main = do

  args <- getArgs

  when (length args == 0) $ error "Needs a filename" 
  
  let fn = args  !! 0
  
  --let f i = (30 :: Int)
  -- Percent/count mapping 
  --let csv = unlines $ [ show x ++ ", " ++ show (f x)  | x <- [1..100]] 


  -- csv <- readFile "leaf_Aug4.csv"
  csv <- readFile fn 
  
  
  -- putStrLn csv 


  -- percent/count mapping
  let the_data = sortBy (\(x,_) (y,_) -> x `compare` y) [ extract l | l <- lines csv]

  let ps  = map fst the_data -- percentages
      cs  = map snd the_data -- counts
      
      totc = sum cs -- total of all "counts"
      
      pcs = map (\x -> 100 * ((fromIntegral x) / (fromIntegral totc))) cs 
      accCount = scanl1 (+) pcs 
  let the_data2 = zip ps accCount :: [(Int,Double)]
        

  putStrLn $ show the_data 
   
  
  -- The data series that generate one set of bars in the same color 
  let the_graph = LineGraph "#000"
                  "Number of functions"
                  Nothing
                  (init the_data2)

  -- The Plot   
  let plot = Plot {pLines = [the_graph],
                   pPoints = [],
                   pBars = [],
                   pLegend = True,
                   pDimensions = (800,400),
                   pXLabel = "Elapsed time difference (%)",
                   pYLabel = "Cumulative % functions"}

                            
                                
  -- Generate the HTML and Javascript code                           
  putStrLn $ html $ renderPlot mySupply plot     

 
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
