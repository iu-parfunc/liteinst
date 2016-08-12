#!/usr/bin/env stack
-- stack --no-system-ghc --verbosity silent --resolver lts-3.8 --install-ghc runghc  --package cassava --package text --package bytestring --package vector --package turtle

-- | This cleans the data from the toggle-throughput test.
--   It filters for data of interest.
--   It adds REQUESTED_TOGGLE_RATE.
--   It adds TOTAL_CALLS if not there.

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module ToggleThroughput_DataCleaner where
import           System.FilePath.Posix
import           System.Environment
import           Data.Text
import           Data.Maybe (catMaybes)
import           GHC.Generics
import           Data.Csv as CSV
import           Data.HashMap.Strict as H
import           Data.ByteString.Char8 as B
import           Data.ByteString.Lazy.Char8 as BL
import           Prelude as P
import qualified Data.Vector as V
import qualified Turtle as T

{-

-- | A throw-away datatype for the record fields we care about in this
-- specific benchmark.
data Record =
     Record
     { progname :: Text
     , variant  :: Text
     , datetime :: Text
     , args     :: Text
    -- ,THREADS,GIT_DEPTH,NUMBER_OF_EXECUTERS,NUMBER_OF_TOGGLES,TOTAL_FOO_CALLS,TOTAL_BAR_CALLS
     }
  deriving (Show,Read,Eq,Ord,Generic)

instance CSV.FromNamedRecord Record
instance CSV.ToNamedRecord   Record
instance CSV.DefaultOrdered  Record

upcaseKeys :: CSV.NamedRecord -> CSV.NamedRecord
upcaseKeys r = [ (upcase k,v) <- H.toList r]

test = upcaseKeys $ CSV.toNamedRecord $ Record "a" "b" "c" "c"

-}

-- Going with the simpler approach:

minGitDepth :: Int
-- minGitDepth = 1600 -- Java ones.
minGitDepth = 1576
-- minGitDepth = 1558
-- Some patch_64 ones are older: 1558,1570,1576

extractToggleRate :: NamedRecord -> Maybe NamedRecord
extractToggleRate r =
     if received >= 0.99 * asked && passFilters
        then Just
           $ insert "REQUESTED_TOGGLE_RATE"
                    (B.pack $ show $ floor asked)
           $ H.adjust (padZero 2)"NUMBER_OF_EXECUTERS"
           $ r'
        else Nothing
  where
  asked, received :: Double
  asked    = read $ B.unpack $ P.last $ B.words (r ! "ARGS")
  received = read $ B.unpack $ r ! "NUMBER_OF_TOGGLES"

  r' = r 
     -- case H.lookup "TOTAL_CALLS" r of
     --     Just x | x /= "" -> r
     --     _ -> insert "TOTAL_CALLS"
     --                 (B.pack $ show
     --                  (read (B.unpack $ r ! "TOTAL_FOO_CALLS") +
     --                   read (B.unpack $ r ! "TOTAL_BAR_CALLS") :: Double))
     --                 r

  -- Any filters to apply to the dataset:
  passFilters =
     (read$ B.unpack$ r ! "GIT_DEPTH") >= minGitDepth &&
     (read$ B.unpack$ r ! "NUMBER_OF_EXECUTERS") <= 15 -- Cutter specific.

padZero :: Int -> B.ByteString -> B.ByteString
padZero num bs =
   let len = B.length bs
   in if len < num
         then B.append (B.pack $ P.replicate (num-len) '0')bs
         else bs

extract :: NamedRecord -> NamedRecord
extract r = new_r 
  where new_r = insert "Executor Threads" (B.pack $ show threads) $
                insert "Toggle Freq" (B.pack $ show freq) $
                insert "Total Calls" (B.pack $ show total_calls) $
                namedRecord [] 

        threads = (read $ B.unpack $ get_threads $ B.words (r ! "ARGS")) :: Int 
        freq    = (read $ B.unpack $ get_freq $ B.words (r ! "ARGS")) :: Int

        total_calls = (read $ B.unpack $ r ! "TOTAL_CALLS") :: Double 
        get_threads [t,_,_] = t
        get_freq    [_,_,f] = f
        
        

main :: IO ()
main =
  do [file] <- getArgs
     bstr   <- BL.readFile file
     let Right (hdr,vec) = CSV.decodeByName bstr
         hdr' = if V.any (== "TOTAL_CALLS") hdr
                   then hdr
                   else hdr V.++ V.singleton "TOTAL_CALLS"
         hdr'' = hdr' V.++ V.singleton "REQUESTED_TOGGLE_RATE"
         bstr2 = CSV.encodeByName hdr''
                                  (P.map extract $ catMaybes $
                                   P.map extractToggleRate $ V.toList vec)
                                  
         outfile  = "./3_cleaned/" ++ takeFileName file
     P.putStrLn $ "Processing "++file++" into "++outfile
     T.mktree "./3_cleaned"
     BL.writeFile outfile bstr2
     return ()
