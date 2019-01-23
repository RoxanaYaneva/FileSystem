module Utils where

import Types

toDirList :: FilePath -> FilePath -> [FilePath]
toDirList "" "/"            = ["/"]
toDirList filePath currPath = "/" : (tail $ split '/' $ relToAbs currPath filePath)

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
findDir' dirs (_:fs)   = findDir' dirs fs

removeFiles :: [FilePath] -> FilePath -> FileSystem -> FileSystem
removeFiles [] _ fs                = fs
removeFiles (arg:args) currPath fs = removeFiles args currPath newFs
    where newFs = FileSystem $ head $ removeFile (toDirList arg currPath) [root fs]

removeFile :: [FilePath] -> [File] -> [File]
removeFile _ [] = []
removeFile [arg] [Directory name lst]
    | arg == name = [Directory name $ removeFile [arg] lst]
    | otherwise   = [Directory name lst]
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
removeDir _ []            = []
removeDir [arg] [Directory name lst]
    | arg == name = [FEmpty]
    | otherwise   = [Directory name lst]
removeDir [arg] ((Directory name lst):fs)
    | arg == name = fs
    | otherwise   = [Directory name $ lst] ++ (removeDir [arg] fs)
removeDir (arg:args) ((Directory name lst):fs)
    | arg == name = [Directory name $ removeDir args lst] ++ fs
    | otherwise   = [Directory name $ lst] ++ (removeDir (arg:args) fs)
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
readFileData fileName []     = do 
    return $ "cat: " ++ fileName ++ ": No such file or directory"
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
showContent' [Directory name _]       = putStrLn $ name ++ "/"
showContent' [OFile name _ size]      = putStrLn name
showContent' ((OFile name _ size):fs) = do
    putStr $ name ++ " "
    showContent' fs
showContent' ((Directory name _):fs)  = do
    putStr $ name ++ "/ "
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

printError :: String -> FilePath -> FileSystem -> IO (FilePath, FileSystem)
printError err currPath fs = do
    putStrLn err
    return (currPath, fs)

relToAbs :: FilePath -> FilePath -> FilePath
relToAbs _ ('/':rel) = "/" ++ rel
relToAbs abs ""      = abs
relToAbs "/" rel     = "/" ++ rel
relToAbs abs rel     = abs ++ "/" ++ rel

goBack :: FilePath -> FilePath
goBack "/"         = "/"
goBack dirs 
    | dirs' == "/" = "/"
    | otherwise    = reverse $ tail dirs'
    where dirs' = dropWhile (/= '/') (reverse dirs)

lastDir :: FilePath -> FilePath
lastDir "/" = "/"
lastDir dirs = head $ reverse $ split '/' dirs   

fileSize :: FileData -> FileSize
fileSize ""  = 0
fileSize text = length text

split :: Char -> String -> [String]
split x y = split' x y [[]]
    where
        split' x [] z = reverse $ map (reverse) z
        split' x (y:ys) (z:zs)
            | y == x    = split' x ys ([]:(z:zs)) 
            | otherwise = split' x ys ((y:z):zs)