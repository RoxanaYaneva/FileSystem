{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Prelude hiding (FilePath, reverse, dropWhile, append, putStrLn, getLine, putStr)
import Data.Text.IO   (putStrLn, getLine, putStr)
import Data.Text      (Text, dropWhile, reverse, splitOn, append, isPrefixOf)
import qualified Data.Text as Text
import Types

toDirList :: FilePath -> FilePath -> [FilePath]
toDirList "" "/"            = ["/"]
toDirList filePath currPath = ["/"] ++ (tail $ splitOn "/" $ relToAbs currPath filePath)

findDir :: FilePath -> FilePath -> FileSystem -> File
findDir "" currPath fs = findDir' (toDirList "" currPath) [root fs]
findDir filePath currPath fs
    | currPath == "/" && filePath == "/" = findDir' ["/"] [root fs]
    | otherwise                          = findDir' (toDirList filePath currPath) [root fs] 

findDir' :: [FilePath] -> [File] -> File
findDir' _ []            = FEmpty
findDir' _ [OFile _ _ _] = FEmpty
findDir' [dir] [Directory name lst]
    | dir == name = Directory name lst
    | otherwise   = FEmpty
findDir' [dir] ((Directory name lst):fs)
    | dir == name = Directory name lst
    | otherwise   = findDir' [dir] fs
findDir' (dir:dirs) ((Directory name lst):fs)
    | dir == name = findDir' dirs lst
    | otherwise   = findDir' (dir:dirs) fs
findDir' dirs (_:fs)    = findDir' dirs fs

createFiles :: [FilePath] -> FilePath -> FileSystem -> IO FileSystem
createFiles [] _ fs = return fs
createFiles (arg:args) currPath fs
    | "" == Text.filter (=='/') arg          = createFiles args currPath newFs
    | not $ isValid (goBack arg) currPath fs = do
        putStrLn $ append "touch: " $ append (goBack arg) ": No such file or directory"
        createFiles args currPath fs
    | otherwise                              = createFiles args currPath newFs
    where newFs = FileSystem $ head $ createFile (toDirList arg currPath) [root fs]

createFile :: [FilePath] -> [File] -> [File]
createFile args fs = createFile' args "" fs

createFile' :: [FilePath] -> FileData -> [File] -> [File]
createFile' _ _ []                                   = []
createFile' [arg] content [FEmpty]                   = [OFile arg content $ fileSize content]
createFile' [arg] content [Directory name lst]       = [Directory name lst] ++ [OFile arg content $ fileSize content]
createFile' [arg] content ((Directory name lst):fs)  = [Directory name lst] ++ (createFile' [arg] content fs)
createFile' [arg] content [OFile name c s]
    | arg == name = [OFile arg content $ fileSize content]
    | otherwise   = [OFile name c s] ++ [OFile arg content $ fileSize content]
createFile' [arg] content ((OFile name c s):fs)
    | arg == name = [OFile name content $ fileSize content] ++ fs
    | otherwise   = [OFile name c s] ++ (createFile' [arg] content fs)
createFile' (arg:args) content ((OFile name c s):fs) = [OFile name c s] ++ (createFile' (arg:args) content fs)
createFile' (arg:args) content ((Directory name lst):fs)
    | arg == name = [Directory name $ createFile' args content lst] ++ fs
    | otherwise   = [Directory name lst] ++ (createFile' (arg:args) content fs)

makeDirs :: [FilePath] -> FilePath -> FileSystem -> IO FileSystem
makeDirs [] _ fs = return fs
makeDirs (arg:args) currPath fs
    | isValid arg currPath fs = do
        putStrLn $ append "mkdir: cannot create directory " $ append arg ": File exists"
        makeDirs args currPath fs
    | otherwise               = makeDirs args currPath newFs
    where newFs = FileSystem $ head $ makeDir (toDirList arg currPath) [root fs]

makeDir :: [FilePath] -> [File] -> [File]
makeDir _ []                        = []
makeDir [arg] [FEmpty]              = [Directory arg [FEmpty]]
makeDir [arg] [Directory name lst]
    | arg == name = [FEmpty]
    | otherwise   = [Directory name lst] ++ [Directory arg [FEmpty]]
makeDir [arg] ((Directory name lst):fs)
    | arg == name = [Directory name lst] ++ fs
    | otherwise   = [Directory name lst] ++ (makeDir [arg] fs)
makeDir [arg] [OFile name c s]      = [OFile name c s] ++ [Directory arg [FEmpty]]
makeDir [arg] ((OFile name c s):fs) = [OFile name c s] ++ (makeDir [arg] fs)
makeDir (arg:args) ((Directory name lst):fs)
    | arg == name = [Directory name $ makeDir args lst] ++ fs
    | otherwise   = [Directory name $ lst] ++ (makeDir (arg:args) fs)
makeDir args ((OFile name c s):fs)  = [OFile name c s] ++ (makeDir args fs)

removeFiles :: [FilePath] -> FilePath -> FileSystem -> IO FileSystem
removeFiles [] _ fs = return fs
removeFiles (arg:args) currPath fs
    | True      = do  -- check if file exists
        putStrLn $ append "rm: failed to remove " $ append arg ": No such file or directory"
        removeFiles args currPath fs
    | otherwise = removeFiles args currPath newFs
    where newFs = FileSystem $ head $ removeFile (toDirList arg currPath) [root fs]

removeFile :: [FilePath] -> [File] -> [File]
removeFile _ []                       = []
removeFile [arg] [Directory name lst] = [Directory name lst]
removeFile (arg:args) ((Directory name lst):fs)
    | arg == name = [Directory name $ safeRemove args lst] ++ fs
    | otherwise   = [Directory name lst] ++ (removeFile (arg:args) fs)
    where safeRemove args lst 
            | removeFile args lst == [] = [FEmpty]
            | otherwise                 = removeFile args lst
removeFile (arg:args) ((OFile name content size):fs)
    | arg == name && args == [] = fs
    | otherwise                 = [OFile name content size] ++ (removeFile (arg:args) fs)

removeDirs :: [FilePath] -> FilePath -> FileSystem -> IO FileSystem
removeDirs [] _ fs = return fs
removeDirs (arg:args) currPath fs
    | not $ isValid arg currPath fs = do
        putStrLn $ append "rmdir: failed to remove " $ append arg ": No such file or directory"
        removeDirs args currPath fs
    | otherwise                     = removeDirs args currPath newFs
    where newFs = FileSystem $ head $ removeDir (toDirList arg currPath) [root fs]

removeDir :: [FilePath] -> [File] -> [File]
removeDir _ []                        = []
removeDir [arg] [Directory name lst]
    | arg == name = []
    | otherwise   = [Directory name lst]
removeDir [arg] ((Directory name lst):fs)
    | arg == name = fs
    | otherwise   = [Directory name lst] ++ (removeDir [arg] fs)
removeDir (arg:args) ((Directory name lst):fs)
    | arg == name = [Directory name $ safeRemoveDir args lst] ++ fs
    | otherwise   = [Directory name lst] ++ (removeDir (arg:args) fs)
    where safeRemoveDir args lst 
            | removeDir args lst == [] = [FEmpty]
            | otherwise                = removeDir args lst
removeDir (arg:args) ((OFile name c s):fs) = [OFile name c s] ++ (removeDir (arg:args) fs)

readAllAndWrite :: [FilePath] -> FilePath -> FilePath -> FileSystem -> IO FileSystem
readAllAndWrite [] _ _ fs = return fs
readAllAndWrite infs outf currPath fs = do
    text <- readAll infs currPath [root fs]
    let rootFs = head $ createFile' (toDirList outf currPath) text [root fs]
    return $ FileSystem rootFs
    where
        readAll [] _ _                 = return ""
        readAll (inf:infs) currPath fs = do
            text <- readFileData (toDirList inf currPath) fs
            all <- readAll infs currPath fs
            return $ append text all 

readFileData :: [FilePath] -> [File] -> IO FileData
readFileData [arg] []    = do
    putStrLn $ append "cat: " $ append arg ": No such file or directory"
    return ""
readFileData [arg] ((OFile name text _):fs)
    | arg == name = return text
    | otherwise   = readFileData [arg] fs
readFileData [arg] ((Directory _ lst):fs) = readFileData [arg] fs
readFileData (arg:args) ((Directory name lst):fs)
    | arg == name = readFileData args lst
    | otherwise   = readFileData (arg:args) fs
readFileData args (_:fs) = readFileData args fs

showContent :: FilePath -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
showContent filePath currPath fs = do 
    let (Directory _ lst) = findDir filePath currPath fs
    showContent' lst
    return (currPath, fs)

showContent' :: [File] -> IO ()
showContent' [FEmpty]                 = return ()
showContent' [Directory name _]       = putStrLn $ append name "/"
showContent' [OFile name _ size]      = putStrLn name
showContent' ((OFile name _ size):fs) = do
    putStr $ append name " "
    showContent' fs
showContent' ((Directory name _):fs)  = do
    putStr $ append name "/ "
    showContent' fs

readFromStdin :: IO ()
readFromStdin = do 
    line <- getLine
    if line == "." then return ()
    else do
        putStrLn line
        readFromStdin

isValid :: FilePath -> FilePath -> FileSystem -> Bool
isValid filePath currPath fs = FEmpty /= findDir filePath currPath fs

printError :: Text -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
printError err currPath fs = do
    putStrLn err
    return (currPath, fs)

relToAbs :: FilePath -> FilePath -> FilePath
relToAbs abs ""          = abs
relToAbs abs rel
    | isPrefixOf "/" rel = rel
    | abs == "/"         = append "/" rel
    | otherwise          = append abs $ append "/" rel  

goBack :: FilePath -> FilePath
goBack "/"         = "/"
goBack dirs
    | dirs' == "/" = "/"
    | otherwise    = reverse $ Text.tail dirs'
    where dirs' = dropWhile (/= '/') (reverse dirs)

lastDir :: FilePath -> FilePath
lastDir "/" = "/"
lastDir dirs = last $ splitOn "/" dirs   

fileSize :: FileData -> FileSize
fileSize text = Text.length text
