module Lib where

import Control.Monad
import Crypto.Hash.SHA256
import qualified Data.ByteString as B
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Posix.Types

listFiles :: FilePath -> String -> IO [FilePath]
listFiles fileExt directory = do
  let fullPath file
        | last directory == '/' = directory ++ file
        | otherwise = directory ++ "/" ++ file
      pred file = null fileExt || takeExtension file == fileExt
  contents <- map fullPath <$> listDirectory directory
  files <- filter pred <$> filterM doesFileExist contents
  dirs <- filterM doesDirectoryExist contents
  dirsFiles <- mapM (listFiles fileExt) dirs
  return $ files ++ concat dirsFiles

getFileSize :: FilePath -> IO FileOffset
getFileSize path = fileSize <$> getFileStatus path

getSHA256 :: FilePath -> IO B.ByteString
getSHA256 path = hash <$> B.readFile path

removeDuplicate :: FilePath -> IO ()
removeDuplicate = removeFile