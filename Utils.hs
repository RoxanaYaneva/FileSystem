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

findDir :: FilePath -> FilePath -> FileSystem -> Maybe File
findDir "" currPath fs = findDir' (toDirList "" currPath) [root fs]
findDir filePath currPath fs
    | currPath == "/" && filePath == "/" = findDir' ["/"] [root fs]
    | otherwise                          = findDir' (toDirList filePath currPath) [root fs] 

findDir' :: [FilePath] -> [File] -> Maybe File
findDir' _ []            = Nothing
findDir' _ [OFile _ _ _] = Nothing
findDir' [dirName] [dir@(Directory name _)]
    | dirName == name = Just dir
    | otherwise       = Nothing
findDir' [dirName] (dir@(Directory name _):fs)
    | dirName == name = Just dir
    | otherwise       = findDir' [dirName] fs
findDir' (dirName:dirs) ((Directory name lst):fs)
    | dirName == name = findDir' dirs lst
    | otherwise       = findDir' (dirName:dirs) fs
findDir' dirs (_:fs)    = findDir' dirs fs

createFiles :: [FilePath] -> FilePath -> FileSystem -> IO FileSystem
createFiles [] _ fs = return fs
createFiles (arg:args) currPath fs
    | "" == Text.filter (=='/') arg          = if isValid arg currPath fs then do 
                                                putStrLn $ append "touch: " $ append arg ": There is such file or directory"
                                                createFiles args currPath fs
                                               else createFiles args currPath newFs
    | not $ isValid (goBack arg) currPath fs = do
        putStrLn $ append "touch: " $ append (goBack arg) ": No such file or directory"
        createFiles args currPath fs
    | otherwise                              = createFiles args currPath newFs
    where newFs = FileSystem $ head $ createFile (toDirList arg currPath) [root fs]

createFile :: [FilePath] -> [File] -> [File]
createFile args fs = createFile' args "" fs

createFile' :: [FilePath] -> FileData -> [File] -> [File]
createFile' [arg] content []                         = [OFile arg content $ fileSize content]
createFile' [arg] content [dir@(Directory _ _)]      = dir : [OFile arg content $ fileSize content]
createFile' [arg] content (dir@(Directory _ _):fs)   = dir : (createFile' [arg] content fs)
createFile' [arg] content [file@(OFile name _ _)]
    | arg == name = [OFile arg content $ fileSize content]
    | otherwise   = file : [OFile arg content $ fileSize content]
createFile' [arg] content (file@(OFile name _ _):fs)
    | arg == name = [OFile name content $ fileSize content] ++ fs
    | otherwise   = file : (createFile' [arg] content fs)
createFile' (arg:args) content (file@(OFile name _ _):fs) = file : (createFile' (arg:args) content fs)
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
makeDir [arg] []              = [Directory arg []]
makeDir [arg] [dir@(Directory name _)]
    | arg == name = []
    | otherwise   = dir : [Directory arg []]
makeDir [arg] (dir@(Directory name _):fs)
    | arg == name = dir : fs
    | otherwise   = dir : (makeDir [arg] fs)
makeDir [arg] [file@(OFile _ _ _)]      = file : [Directory arg []]
makeDir [arg] ((OFile name c s):fs) = [OFile name c s] ++ (makeDir [arg] fs)
makeDir (arg:args) (dir@(Directory name lst):fs)
    | arg == name = [Directory name $ makeDir args lst] ++ fs
    | otherwise   = dir : (makeDir (arg:args) fs)
makeDir args (file@(OFile _ _ _):fs)  = file : (makeDir args fs)

removeFiles :: [FilePath] -> FilePath -> FileSystem -> IO FileSystem
removeFiles [] _ fs = return fs
removeFiles (arg:args) currPath fs
    | False      = do  -- check if file exists
        putStrLn $ append "rm: failed to remove " $ append arg ": No such file or directory"
        removeFiles args currPath fs
    | otherwise = removeFiles args currPath newFs
    where newFs = FileSystem $ head $ removeFile (toDirList arg currPath) [root fs]

removeFile :: [FilePath] -> [File] -> [File]
removeFile _ []                       = []
removeFile [arg] [dir@(Directory _ _)] = [dir]
removeFile (arg:args) (dir@(Directory name lst):fs)
    | arg == name = [Directory name $ removeFile args lst] ++ fs
    | otherwise   = dir : (removeFile (arg:args) fs)
removeFile (arg:args) (file@(OFile name _ _):fs)
    | arg == name && args == [] = fs
    | otherwise                 = file : (removeFile (arg:args) fs)

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
removeDir [arg] [dir@(Directory name _)]
    | arg == name = []
    | otherwise   = [dir]
removeDir [arg] (dir@(Directory name _):fs)
    | arg == name = fs
    | otherwise   = dir : (removeDir [arg] fs)
removeDir (arg:args) (dir@(Directory name lst):fs)
    | arg == name = [Directory name $ removeDir args lst] ++ fs
    | otherwise   = dir : (removeDir (arg:args) fs)
removeDir (arg:args) (file@(OFile _ _ _):fs) = file : (removeDir (arg:args) fs)

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
    putStr $ append "cat: " $ append arg ": No such file or directory"
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
    case findDir filePath currPath fs of
        Just dir -> do
            let (Directory _ lst) = dir
            showContent' lst
        Nothing  -> putStrLn $ "ls: cannot open directory '.': Permission denied"
    return (currPath, fs)

showContent' :: [File] -> IO ()
showContent' []                       = return ()
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
isValid filePath currPath fs = Nothing /= findDir filePath currPath fs

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
