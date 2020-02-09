module Lib where

import Control.Monad
import Data.Map
import System.Directory
import System.Posix.Files
import System.Posix.Types
import Crypto.Hash.SHA256
import qualified Data.ByteString as B

listFiles :: String -> IO [String]
listFiles directory = do
  contents <- getDirectoryContents directory
  filterM doesFileExist contents

getFileSize :: FilePath -> IO FileOffset
getFileSize path = fileSize <$> getFileStatus path

getSHA256 :: FilePath -> IO B.ByteString
getSHA256 path = hash <$> B.readFile path
