module Utils where

import Types

readFromStdin :: IO ()
readFromStdin = do 
    line <- getLine
    if line == "." then return ()
    else do
        putStrLn line
        readFromStdin

goBack :: FilePath -> FilePath
goBack "/"         = "/"
goBack dirs 
    | dirs' == "/" = "/"
    | otherwise    = reverse $ tail dirs'
    where dirs' = dropWhile (/= '/') (reverse dirs)

isValid :: FilePath -> FilePath -> FileSystem -> Bool
isValid filePath currPath fs = FEmpty /= findDir filePath currPath fs

relToAbs :: FilePath -> FilePath -> FilePath
relToAbs "/" rel = "/" ++ rel
relToAbs abs rel = abs ++ "/" ++ rel

findDir :: FilePath -> FilePath -> FileSystem -> FileSystem
findDir "" currPath fs = findDir' ("/" : (tail $ split '/' currPath)) [fs]
findDir filePath currPath fs
    | currPath == "/" && filePath == "/" = findDir' ["/"] [fs]
    | head filePath == '/'               = findDir' ("/" : (tail $ split '/' filePath)) [fs]
    | otherwise                          = findDir' ("/" : (tail $ split '/' $ relToAbs currPath filePath)) [fs]

findDir' :: [FilePath] -> [FileSystem] -> FileSystem
findDir' _ [FEmpty]     = FEmpty
findDir' _ [File _ _ _] = FEmpty
-- findDir' [dir] [Directory name lst]
--     | dir == name = Directory name lst
--     | otherwise   = FEmpty
-- findDir' (dir:dirs) [Directory name lst]
--     | dir == name = findDir' dirs lst
--     | otherwise   = FEmpty
findDir' (dir:dirs) ((Directory name lst):fs)
    | dir == name && dirs /= [] = findDir' dirs lst
    | dir == name && dirs == [] = Directory name lst
    | otherwise                 = findDir' (dir:dirs) fs 
findDir' dirs (_:fs)   = findDir' dirs fs

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

writeToFile :: FileName -> FileData -> FilePath -> [FileSystem] -> FileSystem -- ne e pravilna
writeToFile fileName text targetDir [Directory currDir lst]
    | targetDir == currDir = Directory currDir (lst ++ [File fileName text $ fileSize text])
    | otherwise            = Directory currDir [writeToFile fileName text targetDir lst]

readFileData :: FileName -> [FileSystem] -> IO FileData
readFileData fileName []     = do 
    return $ "cat: " ++ fileName ++ ": No such file or directory"
readFileData fileName ((File name text _):fs)
    | fileName == name = return text
    | otherwise        = readFileData fileName fs
readFileData fileName (_:fs) = readFileData fileName fs

showContent :: [FileSystem] -> IO ()
showContent [FEmpty]                = return ()
showContent [Directory name _]      = putStrLn $ name ++ "/"
showContent [File name _ size]      = putStrLn $ name ++ "  " ++ show size
showContent ((File name _ size):fs) = do
    putStrLn $ name ++ "  " ++ show size
    showContent fs
showContent ((Directory name _):fs) = do
    putStrLn $ name ++ "/"
    showContent fs

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