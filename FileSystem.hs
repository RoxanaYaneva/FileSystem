{-# LANGUAGE OverloadedStrings #-}
module FileSystem where

import Prelude hiding (FilePath)
import Data.Text      (Text)
import Types
import Utils
import Commands

content :: FileData
content = "This is the content. Wow..."

rootDir :: File
rootDir = Directory "/"
            [
                Directory "folder1" 
                    [
                        OFile "file3" content $ fileSize content, 
                        Directory "folder4" []
                    ],
                Directory "folder2" 
                    [
                        Directory "folder3" 
                            [
                                Directory "folder5" []
                            ],
                        OFile "file4" content $ fileSize content
                    ],
                OFile "file1" content $ fileSize content,
                OFile "file2" content $ fileSize content
            ]

fileSystem :: FileSystem
fileSystem = FileSystem rootDir