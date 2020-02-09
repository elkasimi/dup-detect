module Lib where

import Control.Monad
import Crypto.Hash.SHA256
import qualified Data.ByteString as B
import System.Directory
import System.Posix.Files
import System.Posix.Types

listFiles :: FilePath -> IO [FilePath]
listFiles directory = do
  let fullPath file | last directory == '/' = directory ++ file
                    | otherwise = directory ++ "/" ++ file
  contents <- map fullPath <$> listDirectory directory
  files <- filterM doesFileExist contents
  dirs <- filterM doesDirectoryExist contents
  dirsFiles <- mapM listFiles dirs
  return $ files ++ concat dirsFiles

getFileSize :: FilePath -> IO FileOffset
getFileSize path = fileSize <$> getFileStatus path

getSHA256 :: FilePath -> IO B.ByteString
getSHA256 path = hash <$> B.readFile path