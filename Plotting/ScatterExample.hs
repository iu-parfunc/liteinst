

import System.Environment (getArgs)

import HSBencher.Analytics

import Control.Monad 

import Data.List hiding (lines)
import Data.Maybe
import Prelude hiding (log, lines)

-- Temporarily
import Network.Google.FusionTables 
-- import Network.Google (FTString)



  
main :: IO ()
main = do


  let green_points = PointGraph "#0F0"
                                "greens"
                                "not used"
                                ([(2,5),(7,3),(9,15),(10,2)] :: [(Int,Int)])

  
  let blue_points = PointGraph "#00F"
                                "blues"
                                "not used"
                                ([(6,4),(1,4),(2,14),(7,5)] :: [(Int,Int)])


  let plot = Plot {pLines = [],
                   pPoints = [green_points, blue_points],
                   pBars = [],
                   pLegend = True,
                   pDimensions = (800,400),
                   pXLabel = "Benchmark",
                   pYLabel = "Overhead %"} 
                            
                                
  putStrLn "Writing output to ScatterExample.html" 
  writeFile ("ScatterExample.html") $ html $ renderPlot mySupply plot     
