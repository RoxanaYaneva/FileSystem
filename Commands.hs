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
parseCommand cmd cp fs = processCommand (head $ split ' ' cmd) (tail $ split ' ' cmd) cp fs

processCommand :: String -> [FileName] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
processCommand "pwd" _ currPath fs    = pwd currPath fs
processCommand "cd" args currPath fs  = cd args currPath fs
processCommand "ls" args currPath fs  = ls args currPath fs
processCommand "cat" args currPath fs = cat args currPath fs
--processCommand "rm" args currPath fs  = rm args currPath fs
--processCommand "mkdir" args currPath fs = mkdir args currPath fs
--processCommand "rmdir" args currPath fs = rmdir args currPath fs
--processCommand "touch" args currPath fs = touch args currPath fs

pwd :: FilePath -> FileSystem -> IO (FilePath, FileSystem)
pwd currDir fs = do
    putStrLn currDir
    return (currDir, fs)

cd :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
cd [arg] currPath fs
    | arg == ".."                      = return (goBack currPath, fs)
    | isValid arg currPath fs == False = do
        putStrLn $ "cd: " ++ arg ++ ": No such file or directory"
        return (currPath, fs)
    | head arg == '/'                  = return (arg, fs)
    | otherwise                        = return (currPath ++ arg, fs)
cd (arg:args) currPath fs = do
    putStrLn "cd: too many arguments"
    return (currPath, fs)

ls :: [FileName] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
ls [] currPath fs             = do
    let (Directory _ lst) = findDir currPath currPath fs
    showContent lst
    return (currPath, fs)
ls [arg] currPath fs
    | isValid arg currPath fs = do
        showContent $ [findDir arg currPath fs] 
        return (currPath, fs)   
    | otherwise               = do 
        putStrLn $ "ls: cannot access " ++ arg ++ ": No such file or directory"
        return (currPath, fs)
ls args currPath fs           = do
    putStrLn "ls: too many arguments"
    return (currPath, fs)

cat :: [FileName] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
cat [] currPath fs = do
    readFromStdin
    return (currPath, fs)
cat [file] currPath fs = do
    let (Directory _ lst) = findDir (lastDir currPath) currPath fs
    text <- readFileData file lst
    putStrLn text
    return (currPath, fs)
cat (">":file) currPath fs = do
    text <- getLine
    let newFs = writeToFile (head file) text (lastDir currPath) [fs]
    return (currPath, newFs)
--cat args currPath fs = return (currPath, concat args currPath fs)

-------------------------------------------------------------------------------------------

rm :: [FileName] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
rm [] currPath fs   = do
    putStrLn "rm: missing operand"
    return (currPath, fs)
rm args currPath fs = return (currPath, removeFiles args $ lastDir currPath fs) -- check whether file exist

removeFiles :: [FileName] -> FilePath -> FileSystem -> FileSystem
removeFiles [] _ fs = fs
removeFiles (arg:args) targetDir (Directory currDir lst)
    | targetDir == currDir = Directory currDir $ removeFile arg lst
    | otherwise            = Directory currDir $ removeFiles args lst

removeFile :: FileName -> FileSystem -> FileSystem
removeFile _ FEmpty = FEmpty 
removeFile fileName f@(File name data size)
    | fileName == name = FEmpty
    | otherwise        = f
removeFile fileName (Directory name lst) = Directory name $ removeFile fileName lst

{-
rmdir :: [FileName] -> Either IO () IO FileSystem
rmdir []   = Left . putStrLn "rmdir: missing operand"
rmdir args = Right ? filter

removeDir :: FileName -> FileSystem -> IO FileSystem
removeDir dir fs = ?

mkdir :: [FileName] -> FilePath -> FileSystem -> Either (IO (FilePath, FileSystem)) (IO ())
mdkir [] _ _           = Right $ putStrLn "mkdir: missing operand"
mkdir args currPath fs = Left $ return (currPath, )

makeDir :: FileName -> FileSystem -> IO FileSystem -- check if dir already exists
makeDir dir fs = ?
-}