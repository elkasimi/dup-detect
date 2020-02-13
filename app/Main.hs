module Main where

import Control.Monad
import Data.List
import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let (directory, fileExt) =
        case args of
          [a] -> (a, "")
          [a, b] -> (a, b)
  putStrLn $ "Directory set to " ++ directory
  putStrLn "Looking for duplicates .. "
  files <- listFiles fileExt directory
  putStrLn $ "Found " ++ show (length files) ++ " files .."
  let groupedBy fun files = map (map snd) . groupBy (\i j -> fst i == fst j) . sort <$> mapM fun files
      fSize file = do
        size <- getFileSize file
        return (-size, file)
      fHash file = do
        hash <- getSHA256 file
        return (hash, file)
  groupedBySize <- groupedBy fSize files
  forM_ groupedBySize $ \files -> do
    groupedByHash <- filter ((> 1) . length) <$> groupedBy fHash files
    forM_
      groupedByHash
      (\duplicates -> do
         let count = length duplicates
             zipWithIndex = zip [1 ..] :: [a] -> [(Int, a)]
             getInts = map read . words <$> getLine :: IO [Int]
         putStrLn ""
         putStrLn "The following files are duplicates:"
         forM_ (zipWithIndex duplicates) (\(i, file) -> putStrLn $ show i ++ ": " ++ file)
         putStrLn "Please enter the list of files to keep. 0 to keep all"
         filesToKeepIndices <- getInts
         let filesToRemove
               | filesToKeepIndices == [0] = []
               | otherwise = map (\i -> duplicates !! (i - 1)) $ filter (`notElem` filesToKeepIndices) [1 .. count]
         putStrLn "The following files will be removed:"
         forM_ filesToRemove putStrLn
         putStrLn "Proceed? y/n"
         answer <- getLine
         when (answer == "y") $ forM_ filesToRemove removeDuplicate)