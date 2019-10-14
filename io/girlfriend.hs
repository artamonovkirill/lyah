import System.IO  

-- main = do  
--     handle <- openFile "io/girlfriend.txt" ReadMode
--     contents <- hGetContents handle
--     putStr contents
--     hClose handle  

-- main = withFile "io/girlfriend.txt" ReadMode (\h -> do
--         contents <- hGetContents h
--         putStr contents)

main = do   
    withFile "io/girlfriend.txt" ReadMode (\handle -> do  
        hSetBuffering handle $ BlockBuffering $ Just 2048
        contents <- hGetContents handle  
        putStr contents)  

-- main = do
--     contents <- readFile "io/girlfriend.txt"
--     putStr contents

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path mode f = do  
    handle <- openFile path mode   
    result <- f handle  
    hClose handle  
    return result  