module Main where

import Control.Monad
import Data.List
import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let directory = head args
  putStrLn $ "Directory set to " ++ directory
  putStrLn "Looking for duplicates .. "
  files <- listFiles directory
  putStrLn $ "Found " ++ show (length files) ++ " files .."
  let groupedBy fun files = map (map snd) . groupBy (\i j -> fst i == fst j) . sort <$> mapM fun files
      fSize file = do
        size <- getFileSize file
        return (size, file)
      fHash file = do
        hash <- getSHA256 file
        return (hash, file)
  groupedBySize <- groupedBy fSize files
  forM_ groupedBySize $ \files -> do
    groupedByHash <- filter ((> 1) . length) <$> groupedBy fHash files
    forM_
      groupedByHash
      (\duplicates -> do
         putStrLn ""
         putStrLn "The following files are duplicates:"
         forM_ duplicates putStrLn)