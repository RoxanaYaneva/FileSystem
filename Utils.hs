{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Prelude hiding (FilePath, reverse, dropWhile, append, putStrLn, getLine, putStr, cons)
import Data.Text.IO   (putStrLn, getLine, putStr)
import Types
import Data.Text      (Text, dropWhile, reverse, splitOn, append, isPrefixOf, cons)
import qualified Data.Text as Text

toDirList :: FilePath -> FilePath -> [FilePath]
toDirList "" "/"            = ["/"]
toDirList filePath currPath = ["/"] ++ (tail $ splitOn "/" $ relToAbs currPath filePath)

findDir :: FilePath -> FilePath -> FileSystem -> File
findDir "" currPath fs = findDir' (toDirList "" currPath) [root fs]
findDir filePath currPath fs
    | currPath == "/" && filePath == "/" = findDir' ["/"] [root fs]
    | otherwise                          = findDir' (toDirList filePath currPath) [root fs] 

findDir' :: [FilePath] -> [File] -> File
findDir' _ [FEmpty]      = FEmpty
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

createFiles :: [FilePath] -> FilePath -> FileSystem -> FileSystem
createFiles [] _ fs                = fs
createFiles (arg:args) currPath fs = createFiles args currPath newFs
    where newFs = FileSystem $ head $ createFile (toDirList arg currPath) [root fs]

createFile :: [FilePath] -> [File] -> [File]
createFile args fs = createFile' args "" fs

createFile' :: [FilePath] -> FileData -> [File] -> [File]
createFile' _ _ []                                   = []
createFile' [arg] content [Directory name lst]       = [Directory name lst] ++ [OFile arg content $ fileSize content]
createFile' [arg] content ((Directory name lst):fs)  = [Directory name lst] ++ (createFile' [arg] content fs)
createFile' [arg] content [OFile name c s]           = [OFile name c s] ++ [OFile arg content $ fileSize content]
createFile' (arg:args) content ((OFile name c s):fs) = [OFile name c s] ++ (createFile' (arg:args) content fs)
createFile' (arg:args) content ((Directory name lst):fs)
    | arg == name = [Directory name $ createFile' args content lst] ++ fs
    | otherwise   = [Directory name lst] ++ (createFile' (arg:args) content fs)

makeDirs :: [FilePath] -> FilePath -> FileSystem -> FileSystem
makeDirs [] _ fs                = fs
makeDirs (arg:args) currPath fs = makeDirs args currPath newFs
    where newFs = FileSystem $ head $ makeDir (toDirList arg currPath) [root fs]

makeDir :: [FilePath] -> [File] -> [File]
makeDir _ []              = []
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

removeFiles :: [FilePath] -> FilePath -> FileSystem -> FileSystem
removeFiles [] _ fs                = fs
removeFiles (arg:args) currPath fs = removeFiles args currPath newFs
    where newFs = FileSystem $ head $ removeFile (toDirList arg currPath) [root fs]

removeFile :: [FilePath] -> [File] -> [File]
removeFile _ [] = []
removeFile [arg] [Directory name lst] = [Directory name lst]
removeFile (arg:args) ((Directory name lst):fs)
    | arg == name = [Directory name $ removeFile args lst] ++ fs
    | otherwise   = [Directory name lst] ++ (removeFile (arg:args) fs)
removeFile (arg:args) ((OFile name content size):fs)
    | arg == name && args == [] = fs
    | otherwise                 = [OFile name content size] ++ (removeFile (arg:args) fs)

removeDirs :: [FilePath] -> FilePath -> FileSystem -> FileSystem
removeDirs [] _ fs                = fs
removeDirs (arg:args) currPath fs = removeDirs args currPath newFs
    where newFs = FileSystem $ head $ removeDir (toDirList arg currPath) [root fs]

removeDir :: [FilePath] -> [File] -> [File]
removeDir _ []              = []
removeDir [arg] [Directory name lst]
    | arg == name = [FEmpty]
    | otherwise   = [Directory name lst]
removeDir [arg] ((Directory name lst):fs)
    | arg == name = fs
    | otherwise   = [Directory name lst] ++ (removeDir [arg] fs)
removeDir (arg:args) ((Directory name lst):fs)
    | arg == name = [Directory name $ removeDir args lst] ++ fs
    | otherwise   = [Directory name lst] ++ (removeDir (arg:args) fs)
removeDir (arg:args) (_:fs) = removeDir (arg:args) fs

-- readAllAndWrite :: [FilePath] -> FilePath -> FilePath -> FileSystem -> IO FileSystem
-- readAllAndWrite [] _ _ fs = return fs
-- readAllAndWrite (fp:fps) writeFile currPath fs = do
--     let dir = findDir (lastDir currPath) currPath fs
--     if dir /= FEmpty then do
--         let (Directory _ lst) = dir
--         text <- readData fp lst
--         if text == "" then do
--             ufs <- readAllAndWrite fps writeFile currPath fs
--             return ufs
--         else do
--             if isValid writeFile currPath fs then do
--                 let newFs = writeToFile writeFile text fs
--                 ufs <- readAllAndWrite fps writeFile currPath fs
--                 return ufs
--             else do
--                 putStrLn $ "cat: " ++ writeFile ++ ": No such file or directory"
--                 return fs
--     else do
--         ufs <- readAllAndWrite fps writeFile currPath fs
--         return ufs
--
-- writeToFile :: FileName -> FileData -> FilePath -> [File] -> File -- ne e pravilna, check if file exits
-- writeToFile fileName text targetDir [Directory currDir lst]
--     | targetDir == currDir = Directory currDir (lst ++ [OFile fileName text $ fileSize text])
--     | otherwise            = Directory currDir [writeToFile fileName text targetDir lst]

readFileData :: FileName -> [File] -> IO FileData
readFileData fileName []     = return $ append "cat: " $ append fileName ": No such file or directory"
readFileData fileName ((OFile name text _):fs)
    | fileName == name = return text
    | otherwise        = readFileData fileName fs
readFileData fileName (_:fs) = readFileData fileName fs

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
    | isPrefixOf "/" rel = append "/" rel
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
