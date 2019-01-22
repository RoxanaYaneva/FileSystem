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
goBack "/"     = "/"
goBack ('/':_) = "/"
goBack dirs    = tail $ concat $ map (\arg -> "/" ++ arg) $reverse $ tail $ reverse $ split '/' dirs

isValid :: FilePath -> FilePath -> FileSystem -> Bool
isValid filePath currPath fs = FEmpty /= findDir filePath currPath fs

findDir :: FilePath -> FilePath -> FileSystem -> FileSystem
findDir "" currPath fs = findDir' ("/" : (tail $ split '/' currPath)) [fs]
findDir filePath currPath fs
    | currPath == "/"      = findDir' ["/"] [fs]
    | head filePath == '/' = findDir' ("/" : (tail $ split '/' filePath)) [fs]
    | otherwise            = findDir' ("/" : (tail $ split '/' currPath)) [fs]

findDir' :: [FilePath] -> [FileSystem] -> FileSystem
findDir' _ [FEmpty]     = FEmpty
findDir' _ [File _ _ _] = FEmpty
findDir' [dir] [Directory name lst]
    | dir == name = Directory name lst
    | otherwise   = FEmpty
findDir' (dir:dirs) [Directory name lst]
    | dir == name = findDir' dirs lst
    | otherwise   = FEmpty
findDir' (dir:dirs) ((Directory name lst):fs)
    | dir == name && dirs /= [] = findDir' dirs lst
    | dir == name && dirs == [] = Directory name lst
    | otherwise                 = findDir' (dir:dirs) fs 
findDir' dirs (_:lst)   = findDir' dirs lst

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

concatenate :: [FileData] -> FileData
concatenate texts = concat texts

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