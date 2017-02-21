#!/usr/bin/env stack
{- stack
    --no-system-ghc
    --resolver lts-6.10
    --install-ghc runghc
    --package split
    --package containers
-}
    -- --package text
    -- --package aeson
    -- --package json-autotype

{-# OPTIONS_GHC -Wall #-}

-- | Analyze the layouts_*/ files for statistics.
--   Run it on a set of files.

module Main where

import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List as L
-- import Data.Text as T hiding (zip, map, filter)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Read as TR
import Data.Text.IO as TIO hiding (putStrLn)
import Prelude as P
import System.Environment
import System.IO as IO
--------------------------------------------------------------------------------

outFile :: FilePath
outFile = "./layout_table_2.csv"

allPossible :: [Layout]
allPossible = go 0 []
  where
    go n ls | n == 5    = [ls]
            | n >  5    = []
            | otherwise = concat
                          [ go (n+x) (ls++[x])
                          | x <- [1..5] ]

tableLine :: Handle -> [String] -> IO ()
tableLine hnd ls = 
  IO.hPutStrLn hnd $ (concat $ intersperse "," (map id ls))

main :: IO ()
main = do
  ls <- getArgs
  case ls of
    [] -> error "Call the script with some number of layout_*/* files as input."
    files ->
        do
           putStrLn "First, a note on possible layouts:"
           putStrLn$ "There are "++show (length allPossible)++" layouts: "
           print allPossible

           putStrLn$  "Writing results to "++outFile
           hnd <- openFile outFile WriteMode 
           tableLine hnd ["Bench", "probes", "5-byte", "4|1", "3+|.."]

               -- [name, show fives, show fours1, show numArena ]

           txts <- mapM TIO.readFile files
           let parsed = sortDatasets $ map parseFile txts           
           forM_ (zip files parsed) $ \(file,ds) -> do
             putStrLn$ "\nFile: "++file
             analyzeDataset file hnd ds 

           putStrLn "\n\n ALL apps combined:"
           putStrLn "========================================"
           let maps  = map M.fromList parsed
               allds = P.foldl1 (M.unionWith (+)) maps
           analyzeDataset "All" hnd (M.toList allds)

           hClose hnd

type Layout = [Int]
    

-- | The histogram of layout occurrences for a given app.
type Dataset = [(Layout,Int)]

sortDatasets :: [Dataset] -> [Dataset]
sortDatasets dss = map snd $ sort withSizes
  where 
   withSizes = [ (sum (map snd ds), ds) | ds <- dss ]
   
    
analyzeDataset :: String -> Handle -> Dataset -> IO ()
analyzeDataset name hnd ds = do
  let total = sum $ map snd ds
      arena = filter (\(x:_,_) -> x >= 3) ds
      numArena = sum $ map snd arena

  putStrLn $ "Num different layouts: " ++ show (length ds)
  putStrLn $ "All layouts: " ++ show (sort ds)
  putStrLn $ "Layouts NOT found: "++ show
               (S.toList (S.difference (S.fromList allPossible)
                               (S.fromList (map fst ds))))
  putStrLn $ "Total probes: "++ show total

  let ofTotal :: Int -> String
      ofTotal n = show ((round ((fromIntegral n / fromIntegral total) * 100.0 :: Double))::Int) ++"%"
      fives   = sum [ n | ([5],n)   <- ds]
      fours1  = sum [ n | ([4,1],n) <- ds]
      fours2  = sum [ n | ([1,4],n) <- ds]
      pageFillers  = sum [ n | (l,n) <- ds, isPageFiller l ]
      -- RRN: Confirmed this is the same as "arena compliant":
--      pageFillers2 = sum [ n | (l,n) <- ds, bcFree l ]
  putStrLn $ "Arena allocator compliant (3+|*): "++show numArena++", or "++ofTotal numArena           
  putStrLn $ "Ideal 5-byte sites: "++ show fives ++", "++ ofTotal fives
  putStrLn $ "Great 4|1-byte sites: "++ show fours1 ++", "++ ofTotal fours1
  putStrLn $ "Good  1|4-byte sites: "++ show fours2 ++", "++ ofTotal fours2
  putStrLn $ "ABCDE sites where C is free can mostly fill pages: "
               ++ show pageFillers ++", "++ ofTotal pageFillers   
  -- putStrLn $ "ABCDE sites where BC are free can really fill pages (): "
  --              ++ show pageFillers2 ++", "++ ofTotal pageFillers2
  tableLine hnd [name, show total, show fives, show fours1,
                 show numArena++ " "++ofTotal numArena ]
  
  return ()

-- | Is the C byte free?
isPageFiller :: Layout -> Bool
isPageFiller = L.elem C . freeBytes

-- -- | Are B and C both free?
-- bcFree :: Layout -> Bool
-- bcFree l =    B `elem` f
--            && C `elem` f
--   where f = freeBytes l

freeBytes :: Layout -> [Bytes]
freeBytes lay = go [A .. E] lay
  where
    go [] [] = []
    go bytes (n:ns) =
      let (_hd:tl) = take n bytes in
      tl ++ go (drop n bytes) ns
    go b l = error $ "internal error, bad state: "++show (b,l)

data Bytes = A | B | C | D | E deriving (Eq,Show,Ord,Bounded,Enum,Read)
                 
parseFile :: Text -> Dataset
parseFile txt =
  [ case TR.decimal r' of
      Left err -> error $"error parsing occurence count: "++ T.unpack r'++"\n"++err
      Right (n,_) -> ( parseLayout l', n )
  | ln <- T.lines txt
  , let [l,r] = T.split (== ':') ln
        (l',r') = (T.strip l, T.strip r)  
  ]

-- | Parse the "1|3|2" style layout.  Expects a trimmed string with no
-- whitespace.
parseLayout :: Text -> Layout
parseLayout = P.map int . T.split (== '|') 
  where
  int :: Text -> Int 
  int t = case TR.decimal t of
            Left err -> error $ "error parsing, expected number inside layout, got: "++T.unpack t
                              ++"\n"++err
            Right (n,_) -> n

pad :: Int -> String -> String
pad pad s
  | length s < pad = (replicate (pad - length s) ' ') ++ s
  | otherwise      = s


wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
