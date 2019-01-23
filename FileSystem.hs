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
                            Directory "folder1" [(File "file3" content $ fileSize content), (Directory "folder4" [FEmpty])],
                            Directory "folder2" [(Directory "folder3" [(Directory "folder5" [FEmpty])]), (File "file4" content $fileSize content)],
                            File "file1" content $ fileSize content,
                            File "file2" content $ fileSize content
                        ]

main :: IO ()
main = do
    input "/" fileSystem
    return ()
