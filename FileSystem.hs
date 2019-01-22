module FileSystem where

import System.IO
import Types
import Utils
import Commands

content :: FileData
content = "This is the content. Wow..."

fileSystem :: FileSystem
fileSystem = Directory "/"
                        [
                            Directory "folder1" [(File "file3" content $ fileSize content)],
                            Directory "folder2" [FEmpty],
                            File "file1" content $ fileSize content,
                            File "file2" content $ fileSize content
                        ]

main :: IO ()
main = do
    input "/" fileSystem
    return ()
