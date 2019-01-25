{-# LANGUAGE OverloadedStrings #-}
module Commands where

import Prelude hiding (FilePath, append, putStrLn, getLine, putStr)
import Data.Text.IO   (putStrLn, getLine, putStr)
import System.IO      (hFlush, stdout)
import Data.Text      (Text, splitOn, append)
import Types
import Utils

input :: FilePath -> FileSystem -> IO ()
input currPath fs = do
    putStr "$> "
    hFlush stdout
    cmd <- getLine
    if cmd == "" then input currPath fs
    else if cmd == ":q" then return ()
    else do  
        (newPath, newFs) <- parseCommand cmd currPath fs
        input newPath newFs

parseCommand :: Text -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
parseCommand cmd currPath fs = processCommand (head $ splitOn " " cmd) (tail $ splitOn " " cmd) currPath fs

processCommand :: Text -> [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
processCommand "pwd" _ currPath fs      = pwd currPath fs
processCommand "cd" args currPath fs    = cd args currPath fs
processCommand "ls" args currPath fs    = ls args currPath fs
processCommand "cat" args currPath fs   = cat args currPath fs
processCommand "touch" args currPath fs = touch args currPath fs
processCommand "mkdir" args currPath fs = mkdir args currPath fs
processCommand "rm" args currPath fs    = rm args currPath fs
processCommand "rmdir" args currPath fs = rmdir args currPath fs
processCommand cmd _ currPath fs        = printError (append cmd ": command not found") currPath fs

pwd :: FilePath -> FileSystem -> IO (FilePath, FileSystem)
pwd currPath fs = do
    putStrLn currPath
    return (currPath, fs)

cd :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
cd [] currPath fs     = printError "cd: missing operand" currPath fs
cd [".."] currPath fs = return (goBack currPath, fs)
cd [arg] currPath fs
    | not $ isValid arg currPath fs = printError (append "cd: " $ append arg ": No such file or directory") currPath fs
    | otherwise                     = return (relToAbs currPath arg, fs)
cd _ currPath fs      = printError "cd: too many arguments" currPath fs

ls :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
ls [] currPath fs    = showContent currPath currPath fs
ls ["."] currPath fs = showContent currPath currPath fs
ls [arg] currPath fs
    | isValid arg currPath fs = showContent arg currPath fs 
    | otherwise               = printError (append "ls: cannot access " $ append arg ": No such file or directory") currPath fs
ls _ currPath fs = printError "ls: too many arguments" currPath fs

touch :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
touch [] currPath fs   = printError "touch: missing file operand" currPath fs
touch args currPath fs = do
    newFs <- createFiles args currPath fs
    return (currPath, newFs)

mkdir :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
mkdir [] currPath fs   = printError "mkdir: missing operand" currPath fs
mkdir args currPath fs = do
    newFs <- makeDirs args currPath fs
    return (currPath, newFs)

rm :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
rm [] currPath fs   = printError "rm: missing operand" currPath fs
rm args currPath fs = do
    newFs <- removeFiles args currPath fs
    return (currPath, newFs)

rmdir :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
rmdir [] currPath fs   = printError "rmdir: missing operand" currPath fs
rmdir args currPath fs = do
    newFs <- removeDirs args currPath fs
    return (currPath, newFs)
    
cat :: [FilePath] -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
cat [] currPath fs         = do
    readFromStdin
    return (currPath, fs)
cat [arg] currPath fs      = do
    text <- readFileData (toDirList arg currPath) [root fs]
    putStrLn text
    return (currPath, fs)
cat (">":args) currPath fs = do
    text <- getLine
    let rootNewFs = head $ createFile' (toDirList (head args) currPath) text [root fs]
    let newFs = FileSystem rootNewFs
    return (currPath, newFs)    
cat args currPath fs = do
    let filesToRead = takeWhile (/= ">") args
    let fileToWrite = dropWhile (/= ">") args !! 1
    newFs <- readAllAndWrite filesToRead fileToWrite currPath fs
    return (currPath, newFs)