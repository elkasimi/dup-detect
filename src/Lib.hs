module Lib where

import Control.Monad (filterM)
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString as B
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeFile,
  )
import System.FilePath (takeExtension)
import System.Posix.Files (fileSize, getFileStatus)
import System.Posix.Types (FileOffset)

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