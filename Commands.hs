module Commands where

import Types
import Utils

input :: FilePath -> FileSystem -> IO ()
input currPath fs = do 
    putStr "$> "
    cmd <- getLine
    if cmd == "" then input currPath fs
    else if cmd == ":q" then return ()
    else do  
        (newPath, newFs) <- parseCommand cmd currPath fs
        input newPath newFs

parseCommand :: String -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
parseCommand cmd currPath fs = processCommand (head $ split ' ' cmd) (tail $ split ' ' cmd) currPath fs

processCommand :: String -> [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
processCommand "pwd" _ currPath fs      = pwd currPath fs
processCommand "cd" args currPath fs    = cd args currPath fs
processCommand "ls" args currPath fs    = ls args currPath fs
processCommand "cat" args currPath fs   = cat args currPath fs
processCommand "rm" args currPath fs    = rm args currPath fs
processCommand "rmdir" args currPath fs = rmdir args currPath fs
--processCommand "touch" args currPath fs = touch args currPath fs
--processCommand "mkdir" args currPath fs = mkdir args currPath fs

pwd :: FilePath -> FileSystem -> IO (FilePath, FileSystem)
pwd currPath fs = do
    putStrLn currPath
    return (currPath, fs)

cd :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
cd [".."] currPath fs     = return (goBack currPath, fs)
cd [arg] currPath fs
    | not $ isValid arg currPath fs = printError ("cd: " ++ arg ++ ": No such file or directory") currPath fs
    | otherwise                     = return (relToAbs currPath arg, fs)
cd (arg:args) currPath fs = printError "cd: too many arguments" currPath fs

ls :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
ls [] currPath fs = showContent currPath currPath fs
ls [arg] currPath fs
    | isValid arg currPath fs = showContent arg currPath fs 
    | otherwise               = printError ("ls: cannot access " ++ arg ++ ": No such file or directory") currPath fs
ls _ currPath fs = printError "ls: too many arguments" currPath fs

rm :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
rm [] currPath fs   = printError "rm: missing operand" currPath fs
rm args currPath fs = return (currPath, removeFiles args currPath fs)

rmdir :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
rmdir [] currPath fs   = printError "rmdir: missing operand" currPath fs
rmdir args currPath fs = return (currPath, removeDirs args currPath fs)
    
cat :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
cat [] currPath fs    = do
    readFromStdin
    return (currPath, fs)
cat [arg] currPath fs     = do
    let hasPath = [] /= filter (== '/') arg
    if hasPath then do
        let pathToFile = goBack arg
        let (Directory _ lst) = findDir pathToFile currPath fs
        text <- readFileData (lastDir arg) lst
        putStrLn text
    else do
        let (Directory _ lst) = findDir currPath currPath fs
        text <- readFileData arg lst
        putStrLn text
    return (currPath, fs)
-- cat (">":args) currPath fs = do
--     let arg = head args
--     let hasPath = [] /= filter (== '/') arg
--     if hasPath then do
--         let pathToFile = goBack arg
--         let dir = findDir pathToFile currPath fs
--         if dir == FEmpty then do
--             putStrLn $ "cat: " ++ arg ++ ": No such file or directory"
--             return (currPath, fs)
--         else do
--             text <- getLine
--             let newFs = writeToFile (lastDir arg) text pathToFile [fs] -- tuk neshto ne raboti
--             return (currPath, newFs)
--     else do
--         text <- getLine
--         let newFs = writeToFile arg text (lastDir currPath) [fs]
--         return (currPath, newFs)       
-- cat args currPath fs = do
--     let filesToRead = takeWhile (/= ">") args
--     let fileToWrite = dropWhile (/= ">") args !! 1
--     newFs <- readAndWrite filesToRead fileToWrite currPath fs
--     return (currPath, newFs)
